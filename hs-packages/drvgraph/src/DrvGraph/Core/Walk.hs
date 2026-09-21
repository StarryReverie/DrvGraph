module DrvGraph.Core.Walk
    ( walk
    ) where

import Control.Exception.Safe (MonadCatch, MonadThrow)
import Control.Monad (forM)
import Data.Function ((&))
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Optics ((%~), (^.))
import Optics.TH (makeFieldLabelsNoPrefix)

import DrvGraph.Core.Capability.CapDerivation (CapDerivation)
import DrvGraph.Core.Capability.CapDerivation qualified as CapDerivation
import DrvGraph.Core.Capability.CapStoreObject (CapStoreObject)
import DrvGraph.Core.Capability.CapStoreObject qualified as CapStoreObject
import DrvGraph.Core.Capability.CapTaskExecutor (CapTaskExecutor)
import DrvGraph.Core.Capability.CapTaskExecutor qualified as CapTaskExecutor
import DrvGraph.Core.Error (checkpointAppError, throwAppErrorText)
import DrvGraph.Core.Model.DepGraph (DepGraph, DrvNode (..), ObjNode (ObjExisted, ObjUnbuilt, ObjUnsynced))
import DrvGraph.Core.Model.DepGraph qualified as DepGraph
import DrvGraph.Core.Model.Derivation (Derivation (..), DerivationOutput (..))
import DrvGraph.Core.Model.DerivingPath (DerivingPath)
import DrvGraph.Core.Model.DerivingPath qualified as DerivingPath
import DrvGraph.Core.Model.NarInfo (NarInfo (..))
import DrvGraph.Core.Model.StoreObjectPath (StoreObjectPath)
import DrvGraph.Core.Model.StoreObjectPath qualified as StoreObjectPath

data WalkState = WalkState
    { depGraph :: DepGraph
    , toVisit :: Set QueuedElement
    , visited :: Set VisitedElement
    }

data QueuedElement
    = QueElemDrvOut
        { drvPath :: DerivingPath
        , outName :: Text
        }
    | QueElemObjWithDrv
        { objPath :: StoreObjectPath
        , drvPath :: DerivingPath
        }
    | QueElemObj
        { objPath :: StoreObjectPath
        }
    | QueElemDrv
        { drvPath :: DerivingPath
        }
    deriving (Eq, Ord, Show)

data VisitedElement
    = VisElemDrvOut
        { drvPath :: DerivingPath
        , outName :: Text
        }
    | VisElemObj
        { objPath :: StoreObjectPath
        }
    | VisElemDrv
        { drvPath :: DerivingPath
        }
    deriving (Eq, Ord, Show)

data StepResult
    = StepResDrvOut
        { nexts :: Set QueuedElement
        }
    | StepResObj
        { objPath :: StoreObjectPath
        , objNode :: ObjNode
        , nexts :: Set QueuedElement
        }
    | StepResDrv
        { drvPath :: DerivingPath
        , drvNode :: DrvNode
        , nexts :: Set QueuedElement
        }
    deriving (Eq, Show)

makeFieldLabelsNoPrefix ''WalkState

-- | Traverse the Nix store from a derivation's output, which coresponds to a
-- store object path uniquely. Returns the dependency graph and the store object
-- path of the given @(DerivingPath, Text)@ pair.
walk
    :: (CapDerivation m, CapStoreObject m, CapTaskExecutor m, MonadCatch m)
    => FilePath -> DerivingPath -> Text -> m (DepGraph, StoreObjectPath)
walk storeDir drvPath outName = do
    let initial =
            WalkState
                { depGraph = DepGraph.empty
                , toVisit = Set.singleton (QueElemDrvOut{drvPath, outName})
                , visited = Set.empty
                }

    WalkState{depGraph} <- CapTaskExecutor.withTaskExecutor $ walkLoop storeDir initial

    objPath <- do
        drv <- loadDrv storeDir drvPath
        lookupOut drvPath drv outName

    pure (depGraph, objPath)

walkLoop
    :: (CapDerivation m, CapStoreObject m, MonadCatch m)
    => FilePath -> WalkState -> (m StepResult -> m (), m (Maybe StepResult)) -> m WalkState
walkLoop storeDir state (submit, await) = case popFront state of
    (Nothing, newState) ->
        await >>= \case
            Nothing -> pure newState
            Just res -> do
                let nextState = mergeResult newState res
                walkLoop storeDir nextState (submit, await)
    (Just front, newState) -> do
        submit $ runStep storeDir front
        walkLoop storeDir newState (submit, await)

popFront :: WalkState -> (Maybe QueuedElement, WalkState)
popFront state = case Set.toList (Set.take 1 (state ^. #toVisit)) of
    [] -> (Nothing, state)
    front : _ ->
        let visFront = queuedToVisitedElem front
        in  if Set.member visFront (state ^. #visited)
                then popFront (state & #toVisit %~ Set.drop 1)
                else
                    ( Just front
                    , state
                        & #toVisit %~ Set.drop 1
                        & #visited %~ Set.insert visFront
                    )

queuedToVisitedElem :: QueuedElement -> VisitedElement
queuedToVisitedElem QueElemDrvOut{drvPath, outName} = VisElemDrvOut{drvPath, outName}
queuedToVisitedElem QueElemObjWithDrv{objPath} = VisElemObj{objPath}
queuedToVisitedElem QueElemObj{objPath} = VisElemObj{objPath}
queuedToVisitedElem QueElemDrv{drvPath} = VisElemDrv{drvPath}

mergeResult :: WalkState -> StepResult -> WalkState
mergeResult state res = case res of
    StepResDrvOut{nexts} -> state & #toVisit %~ Set.union nexts
    StepResObj{objPath, objNode, nexts} ->
        state
            & #toVisit %~ Set.union nexts
            & #depGraph %~ DepGraph.insertObjNode objPath objNode
    StepResDrv{drvPath, drvNode, nexts} ->
        state
            & #toVisit %~ Set.union nexts
            & #depGraph %~ DepGraph.insertDrvNode drvPath drvNode

runStep
    :: (CapDerivation m, CapStoreObject m, MonadCatch m)
    => FilePath -> QueuedElement -> m StepResult
runStep storeDir queElem = case queElem of
    QueElemDrvOut{drvPath, outName} -> runStepDrvOut storeDir drvPath outName
    QueElemObjWithDrv{objPath, drvPath} -> runStepObjWithDrv storeDir objPath drvPath
    QueElemObj{objPath} -> runStepObj storeDir objPath
    QueElemDrv{drvPath} -> runStepDrv storeDir drvPath

runStepDrvOut
    :: (CapDerivation m, MonadCatch m)
    => FilePath -> DerivingPath -> Text -> m StepResult
runStepDrvOut storeDir drvPath outName = do
    drv <- loadDrv storeDir drvPath
    objPath <- lookupOut drvPath drv outName
    let next = QueElemObjWithDrv{objPath, drvPath}
    pure StepResDrvOut{nexts = Set.singleton next}

runStepObjWithDrv
    :: (CapStoreObject m, MonadCatch m)
    => FilePath -> StoreObjectPath -> DerivingPath -> m StepResult
runStepObjWithDrv storeDir objPath drvPath = do
    maybeObjNode <- queryObjNode storeDir objPath
    case maybeObjNode of
        Just (objNode, nexts) -> pure StepResObj{objPath, objNode, nexts}
        Nothing -> do
            let objNode = ObjUnbuilt{drvPath}
            let nexts = Set.singleton QueElemDrv{drvPath}
            pure StepResObj{objPath, objNode, nexts}

runStepObj
    :: (CapStoreObject m, MonadCatch m)
    => FilePath -> StoreObjectPath -> m StepResult
runStepObj storeDir objPath = do
    maybeObjNode <- queryObjNode storeDir objPath
    case maybeObjNode of
        Just (objNode, nexts) -> pure StepResObj{objPath, objNode, nexts}
        Nothing -> do
            let opText = StoreObjectPath.toText objPath
            throwAppErrorText $ "no narinfo for " <> opText <> ", don't know how to build"

runStepDrv
    :: (CapDerivation m, MonadCatch m)
    => FilePath -> DerivingPath -> m StepResult
runStepDrv storeDir drvPath = do
    drv <- loadDrv storeDir drvPath
    paths <- resolveDrvInputObjPaths storeDir drvPath drv
    let drvNode = DrvNode{inputObjPaths = Map.keysSet paths}
    let nexts = Set.fromList $ uncurry QueElemDrvOut <$> Map.elems paths
    pure StepResDrv{drvPath, drvNode, nexts}

resolveDrvInputObjPaths
    :: (CapDerivation m, MonadCatch m)
    => FilePath
    -> DerivingPath
    -> Derivation
    -> m (Map StoreObjectPath (DerivingPath, Text))
resolveDrvInputObjPaths storeDir drvPath drv = do
    let realInputDrvs = List.filter isNotSelf . Map.toList $ drv ^. #inputDrvs
          where
            isNotSelf (p, _) = p /= drvPath

    inputs <- forM realInputDrvs $ \(inputDrvPath, outNames) -> do
        inputDrv <- loadDrv storeDir inputDrvPath
        pure (inputDrvPath, inputDrv, outNames)

    inputObjPaths <- forM inputs $ \(inputDrvPath, inputDrv, outNames) ->
        forM (Set.toList outNames) $ \outName ->
            case Map.lookup outName (inputDrv ^. #outputs) of
                Just DerivationOutput{path} -> pure (path, (inputDrvPath, outName))
                Nothing -> do
                    let dp = DerivingPath.toText inputDrvPath
                    throwAppErrorText $ "derivation " <> dp <> " doesn't output " <> outName

    pure $ Map.fromList (concat inputObjPaths)

queryObjNode
    :: (CapStoreObject m, MonadCatch m)
    => FilePath
    -> StoreObjectPath
    -> m (Maybe (ObjNode, Set QueuedElement))
queryObjNode storeDir objPath = do
    let opText = StoreObjectPath.toText objPath

    isSynced <- do
        let errMsg = "could not query local store object path " <> opText
        checkpointAppError errMsg $ CapStoreObject.queryLocalStoreObject storeDir objPath

    if isSynced
        then pure $ Just (ObjExisted, Set.empty)
        else do
            maybeNarInfo <- do
                let errMsg = "could not query remote store object path" <> opText
                checkpointAppError errMsg $ CapStoreObject.queryRemoteStoreObject objPath

            pure $ flip fmap maybeNarInfo $ \NarInfo{references = refPaths, deriver} ->
                let objNode = ObjUnsynced{refPaths, deriver}
                    nexts = Set.map (\p -> QueElemObj{objPath = p}) refPaths
                in  (objNode, nexts)

lookupOut
    :: (MonadThrow m)
    => DerivingPath -> Derivation -> Text -> m StoreObjectPath
lookupOut drvPath drv outName =
    case Map.lookup outName $ drv ^. #outputs of
        Just output -> pure $ output ^. #path
        Nothing -> do
            let dp = DerivingPath.toText drvPath
            throwAppErrorText $ "derivation " <> dp <> " doesn't contain output " <> outName

loadDrv
    :: (CapDerivation m, MonadCatch m)
    => FilePath -> DerivingPath -> m Derivation
loadDrv storeDir drvPath = do
    let dpText = DerivingPath.toText drvPath
    checkpointAppError ("could not load derivation " <> dpText) $ do
        CapDerivation.loadDerivation storeDir drvPath
