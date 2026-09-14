module DrvGraph.Core.Walk
    ( walk
    ) where

import Control.Monad (forM)
import Control.Monad.Except (throwError)
import Data.Function ((&))
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Optics ((%~), (.~), (^.))
import Optics.TH (makeFieldLabelsNoPrefix)

import DrvGraph.Core.Capability.CapDerivation (CapDerivation)
import DrvGraph.Core.Capability.CapDerivation qualified as CapDerivation
import DrvGraph.Core.Capability.CapStoreObject (CapStoreObject, NarInfo (..))
import DrvGraph.Core.Capability.CapStoreObject qualified as CapStoreObject
import DrvGraph.Core.Error (AppExceptT, appError, withErrContext)
import DrvGraph.Core.Model.DepGraph (DepGraph, DrvNode (..), ObjNode (ObjExisted, ObjUnbuilt, ObjUnsynced))
import DrvGraph.Core.Model.DepGraph qualified as DepGraph
import DrvGraph.Core.Model.Derivation (Derivation (..), DerivationOutput (..))
import DrvGraph.Core.Model.DerivingPath (DerivingPath)
import DrvGraph.Core.Model.DerivingPath qualified as DerivingPath
import DrvGraph.Core.Model.StoreObjectPath (StoreObjectPath)
import DrvGraph.Core.Model.StoreObjectPath qualified as StoreObjectPath

data WalkState = WalkState
    { depGraph :: DepGraph
    , visitQueue :: Set QueueElement
    , visited :: Set QueueElement
    }

data QueueElement
    = QueElemDrvOut
        { drvPath :: DerivingPath
        , outName :: Text
        }
    | QueElemObj
        { objPath :: StoreObjectPath
        }
    deriving (Eq, Ord, Show)

data StepDrvNodeResult = StepDrvNodeResult
    { drvNode :: Maybe DrvNode
    , objNodePair :: Maybe (StoreObjectPath, ObjNode)
    , nextQueElems :: Set QueueElement
    }

makeFieldLabelsNoPrefix ''WalkState
makeFieldLabelsNoPrefix ''QueueElement
makeFieldLabelsNoPrefix ''StepDrvNodeResult

-- | Traverse the Nix store from a derivation's output, which coresponds to a
-- store object path uniquely. Returns the dependency graph and the store object
-- path of the given @(DerivingPath, Text)@ pair.
walk
    :: (CapDerivation m, CapStoreObject m)
    => FilePath -> DerivingPath -> Text -> AppExceptT m (DepGraph, StoreObjectPath)
walk storeDir drvPath outName = do
    let initial =
            WalkState
                { depGraph = DepGraph.empty
                , visitQueue = Set.singleton (QueElemDrvOut{drvPath, outName})
                , visited = Set.empty
                }
    WalkState{depGraph} <- walkLoop storeDir initial
    objPath <- do
        drv <- loadDrv storeDir drvPath
        lookupOut drvPath drv outName

    pure (depGraph, objPath)

walkLoop
    :: (CapDerivation m, CapStoreObject m)
    => FilePath -> WalkState -> AppExceptT m WalkState
walkLoop storeDir state = do
    (fronts, poppedState) <- do
        let (fronts, poppedQueue) = popQueueFront 1 (state ^. #visitQueue)
        let poppedState = state & #visitQueue .~ poppedQueue
        pure (fronts, poppedState)

    case fronts of
        [] -> pure state
        [front] -> do
            nextState <- walkLoopStep storeDir front poppedState
            walkLoop storeDir nextState
        _ -> error "todo: change to concurrent traversal"

popQueueFront :: Int -> Set a -> ([a], Set a)
popQueueFront num queue = (Set.toList (Set.take num queue), Set.drop num queue)

walkLoopStep
    :: (CapDerivation m, CapStoreObject m)
    => FilePath
    -> QueueElement
    -> WalkState
    -> AppExceptT m WalkState
walkLoopStep storeDir front state = do
    if Set.member front (state ^. #visited)
        then pure state
        else runUnvisited
  where
    runUnvisited = do
        let visitedState = state & #visited %~ Set.insert front
        case front of
            QueElemDrvOut{drvPath, outName} -> do
                StepDrvNodeResult{drvNode, objNodePair, nextQueElems} <-
                    stepDrvNode storeDir drvPath outName state
                pure $
                    visitedState
                        & #depGraph %~ opMaybe drvNode (DepGraph.insertDrvNode drvPath)
                        & #depGraph %~ opMaybe objNodePair (uncurry DepGraph.insertObjNode)
                        & #visitQueue %~ Set.union nextQueElems
            QueElemObj{objPath} -> do
                (objNode, nextQueElems) <- stepObjNode storeDir objPath state
                pure $
                    visitedState
                        & #depGraph %~ DepGraph.insertObjNode objPath objNode
                        & #visitQueue %~ Set.union nextQueElems

    opMaybe (Just val) op = op val
    opMaybe Nothing _ = id

stepDrvNode
    :: (CapDerivation m, CapStoreObject m)
    => FilePath
    -> DerivingPath
    -> Text
    -> WalkState
    -> AppExceptT m StepDrvNodeResult
stepDrvNode storeDir drvPath outName env = do
    drv <- loadDrv storeDir drvPath
    objPath <- lookupOut drvPath drv outName

    if Maybe.isJust $ DepGraph.lookupObjNode objPath (env ^. #depGraph)
        then pure emptyRes
        else goQueryObj objPath drv
  where
    goQueryObj objPath drv = do
        maybeObjRes <- queryObjNode storeDir objPath env
        case maybeObjRes of
            Just (objNode, nextQueElems) ->
                pure emptyRes{objNodePair = Just (objPath, objNode), nextQueElems}
            Nothing ->
                if Maybe.isJust $ DepGraph.lookupDrvNode drvPath (env ^. #depGraph)
                    then pure emptyRes{objNodePair = Just (objPath, ObjUnbuilt{drvPath})}
                    else goDrvNode objPath drv

    goDrvNode objPath drv = do
        paths <- resolveDrvInputObjPaths storeDir drvPath drv
        pure
            StepDrvNodeResult
                { drvNode = Just DrvNode{inputObjPaths = Map.keysSet paths}
                , objNodePair = Just (objPath, ObjUnbuilt{drvPath})
                , nextQueElems =
                    Map.elems paths
                        & fmap (\(dp, out) -> QueElemDrvOut{drvPath = dp, outName = out})
                        & Set.fromList
                }

    emptyRes =
        StepDrvNodeResult
            { drvNode = Nothing
            , objNodePair = Nothing
            , nextQueElems = Set.empty
            }

resolveDrvInputObjPaths
    :: (CapDerivation m)
    => FilePath
    -> DerivingPath
    -> Derivation
    -> AppExceptT m (Map StoreObjectPath (DerivingPath, Text))
resolveDrvInputObjPaths storeDir drvPath drv = do
    let realInputDrvs = List.filter isSelf . Map.toList $ drv ^. #inputDrvs
          where
            isSelf (p, _) = p /= drvPath

    inputs <- forM realInputDrvs $ \(inputDrvPath, outNames) -> do
        inputDrv <- loadDrv storeDir inputDrvPath
        pure (inputDrvPath, inputDrv, outNames)

    inputObjPaths <- forM inputs $ \(inputDrvPath, inputDrv, outNames) -> do
        let outs = inputDrv ^. #outputs
        forM (Set.toList outNames) $ \outName -> do
            case Map.lookup outName outs of
                Just DerivationOutput{path} -> pure (path, (inputDrvPath, outName))
                Nothing -> do
                    let dp = DerivingPath.toText inputDrvPath
                    let errMsg = "derivation " <> dp <> " doesn't output " <> outName
                    throwError $ appError errMsg

    pure $ Map.fromList (concat inputObjPaths)

stepObjNode
    :: (CapStoreObject m)
    => FilePath
    -> StoreObjectPath
    -> WalkState
    -> AppExceptT m (ObjNode, Set QueueElement)
stepObjNode storeDir objPath env = do
    res <- queryObjNode storeDir objPath env
    case res of
        Just inner -> pure inner
        Nothing -> do
            let opText = StoreObjectPath.toText objPath
            let errMsg = "no narinfo for " <> opText <> ", don't know how to build"
            throwError $ appError errMsg

queryObjNode
    :: (CapStoreObject m)
    => FilePath
    -> StoreObjectPath
    -> WalkState
    -> AppExceptT m (Maybe (ObjNode, Set QueueElement))
queryObjNode storeDir objPath env = do
    let opText = StoreObjectPath.toText objPath

    isSynced <- do
        let errMsg = "could not query local store object path " <> opText
        withErrContext errMsg $ CapStoreObject.queryLocalStoreObject storeDir objPath

    if isSynced
        then pure $ Just (ObjExisted, Set.empty)
        else do
            maybeNarInfo <- do
                let errMsg = "could not query remote store object path" <> opText
                withErrContext errMsg $ CapStoreObject.queryRemoteStoreObject objPath

            pure $ flip fmap maybeNarInfo $ \NarInfo{narInfoRefs = refPaths} ->
                let objNode = ObjUnsynced{refPaths}
                    nexts =
                        refPaths
                            & Set.map (\p -> QueElemObj{objPath = p})
                            & Set.filter (\e -> not $ Set.member e (env ^. #visited))
                in  (objNode, nexts)

lookupOut :: (Monad m) => DerivingPath -> Derivation -> Text -> AppExceptT m StoreObjectPath
lookupOut drvPath drv outName =
    case Map.lookup outName $ drv ^. #outputs of
        Just output -> pure $ output ^. #path
        Nothing -> do
            let dp = DerivingPath.toText drvPath
            let errMsg = "derivation " <> dp <> " doesn't contain output " <> outName
            throwError $ appError errMsg

loadDrv
    :: (CapDerivation m)
    => FilePath
    -> DerivingPath
    -> AppExceptT m Derivation
loadDrv storeDir drvPath = do
    let dpText = DerivingPath.toText drvPath
    withErrContext ("could not load derivation " <> dpText) $ do
        CapDerivation.loadDerivation storeDir drvPath
