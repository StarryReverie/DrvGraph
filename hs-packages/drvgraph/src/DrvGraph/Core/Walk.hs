module DrvGraph.Core.Walk
    ( walk
    ) where

import Control.Monad (forM, forM_)
import Control.Monad.Except (throwError)
import Control.Monad.State.Strict (StateT (..), gets, modify)
import Control.Monad.Trans (lift)
import Data.Foldable (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Optics ((%~), (^.))
import Optics.TH (makeFieldLabelsNoPrefix)

import DrvGraph.Core.Capability.CapDerivation (CapDerivation)
import DrvGraph.Core.Capability.CapDerivation qualified as CapDerivation
import DrvGraph.Core.Capability.CapStoreObject (CapStoreObject, NarInfo (..))
import DrvGraph.Core.Capability.CapStoreObject qualified as CapStoreObject
import DrvGraph.Core.Error (AppExceptT, appError, withErrContext)
import DrvGraph.Core.Model.DepGraph (DepGraph, DrvNode (..), ObjNode (ObjExisted, ObjUnbuilt, ObjUnsynced))
import DrvGraph.Core.Model.DepGraph qualified as DepGraph
import DrvGraph.Core.Model.Derivation (Derivation (..), DerivationOutput (..))
import DrvGraph.Core.Model.Derivation qualified as Derivation
import DrvGraph.Core.Model.DerivingPath (DerivingPath)
import DrvGraph.Core.Model.DerivingPath qualified as DerivingPath
import DrvGraph.Core.Model.StoreObjectPath (StoreObjectPath)
import DrvGraph.Core.Model.StoreObjectPath qualified as StoreObjectPath

data WalkState = WalkState
    { loadedDrvCache :: Map DerivingPath Derivation
    , depGraph :: DepGraph
    }

makeFieldLabelsNoPrefix ''WalkState

-- | Traverse the Nix store from a derivation's output, which coresponds to a
-- store object path uniquely. Returns the dependency graph and the store object
-- path of the given @(DerivingPath, Text)@ pair.
walk
    :: (CapDerivation m, CapStoreObject m)
    => FilePath -> DerivingPath -> Text -> AppExceptT m (DepGraph, StoreObjectPath)
walk storeDir drvPath outName = do
    let initial = WalkState{loadedDrvCache = Map.empty, depGraph = DepGraph.empty}
    (objPath, WalkState{depGraph}) <- runStateT (walkImpl storeDir drvPath outName) initial
    pure (depGraph, objPath)

type CurrentT m = StateT WalkState (AppExceptT m)

walkImpl
    :: (CapDerivation m, CapStoreObject m)
    => FilePath -> DerivingPath -> Text -> CurrentT m StoreObjectPath
walkImpl storeDir drvPath outName = do
    -- Get the @Derivation@ at @drvPath@.
    drv <- ensureDerivation drvPath (CapDerivation.loadDerivation storeDir)

    -- Insert @drv@'s @DrvNode@ to the dependency graph, if not done yet.
    depGraph <- gets (^. #depGraph)
    drvNode <- case DepGraph.lookupDrvNode drvPath depGraph of
        Just drvNode -> pure drvNode
        Nothing -> do
            inputObjPaths <- resolveDrvInputObjPaths storeDir drv
            let drvNode = DrvNode{inputObjPaths}
            modify $ #depGraph %~ DepGraph.insertDrvNode drvPath drvNode
            pure drvNode

    -- Get the @StoreObjectPath@ of @drvPath@^@outName@.
    stObjPath <- case Map.lookup outName $ drv ^. #outputs of
        Just output -> do
            pure $ output ^. #path
        Nothing -> do
            let dp = DerivingPath.toText drvPath
            let errMsg = "derivation " <> dp <> " doesn't contain output " <> outName
            throwError $ appError errMsg

    -- Check whether we have visited the current store object path. If not,
    -- process it and recurse into its dependencies.
    maybeVisitedObj <- gets $ DepGraph.lookupObjNode stObjPath . (^. #depGraph)
    case maybeVisitedObj of
        Just _ -> pure ()
        Nothing -> resolveObjNodeAndRecurse storeDir drvPath stObjPath drvNode drv

    pure stObjPath

resolveObjNodeAndRecurse
    :: (CapDerivation m, CapStoreObject m)
    => FilePath -> DerivingPath -> StoreObjectPath -> DrvNode -> Derivation -> CurrentT m ()
resolveObjNodeAndRecurse storeDir drvPath stObjPath drvNode drv = do
    isSynced <- do
        let errMsg = "could not query local store object path" <> StoreObjectPath.toText stObjPath
        lift $ withErrContext errMsg $ CapStoreObject.queryLocalStoreObject storeDir stObjPath

    if isSynced
        -- Stop at already existed store object path, whose its closure
        -- exists as well.
        then do
            modify $ #depGraph %~ DepGraph.insertObjNode stObjPath ObjExisted
        else do
            maybeNarInfo <- do
                let errMsg = "could not query remote store object path" <> StoreObjectPath.toText stObjPath
                lift $ withErrContext errMsg $ CapStoreObject.queryRemoteStoreObject stObjPath

            -- Get the list of dependent input derivation outputs to recurse into.
            let DrvNode{inputObjPaths} = drvNode
            inputDrvs <- case maybeNarInfo of
                Just NarInfo{narInfoRefs = refPaths} -> do
                    -- Store object in remote binary caches only needs runtime
                    -- dependencies included in its narinfo's @References@ line.
                    let objNode = ObjUnsynced{drvPath, refPaths}
                    modify $ #depGraph %~ DepGraph.insertObjNode stObjPath objNode
                    collectInputDrvsFromRefs storeDir refPaths inputObjPaths drv
                Nothing -> do
                    -- Otherwise Nix needs to build this derivation locally, so
                    -- all input derivation outputs are required.
                    let objNode = ObjUnbuilt{drvPath}
                    modify $ #depGraph %~ DepGraph.insertObjNode stObjPath objNode
                    pure $ Map.elems inputObjPaths

            -- Recurse into dependencies.
            forM_ inputDrvs $ \(inputDrv, inputDrvOutName) -> do
                walkImpl storeDir inputDrv inputDrvOutName

collectInputDrvsFromRefs
    :: (CapDerivation m)
    => FilePath
    -> Set StoreObjectPath
    -> Map StoreObjectPath (DerivingPath, Text)
    -> Derivation
    -> CurrentT m [(DerivingPath, Text)]
collectInputDrvsFromRefs storeDir refPaths inputObjPaths drv = do
    maybeInputDrvs <- forM (Set.toList refPaths) $ \refPath -> do
        let inSelf = Derivation.existsOutput refPath drv
        let inDirectInputs = Map.lookup refPath inputObjPaths

        case (inSelf, inDirectInputs) of
            (True, _) -> do
                -- If this store path is an output of the derivation itself,
                -- don't recurse into itself.
                pure Nothing
            (False, Just inputDrv) -> pure $ Just inputDrv
            (False, Nothing) -> do
                -- Some references are propagated inputs, which can't be
                -- directly found in the current derivation's inputs.
                res <- queryDerivationAndOutputByObjPath storeDir refPath
                pure $ Just res

    pure $ catMaybes maybeInputDrvs

queryDerivationAndOutputByObjPath
    :: (CapDerivation m)
    => FilePath -> StoreObjectPath -> CurrentT m (DerivingPath, Text)
queryDerivationAndOutputByObjPath storeDir objPath = do
    maybeDrvPath <- do
        let errMsg = "could not query deriver for " <> StoreObjectPath.toText objPath
        lift $ withErrContext errMsg $ CapDerivation.queryDeriver storeDir objPath

    drvPath <- case maybeDrvPath of
        Just drvPath -> pure drvPath
        Nothing -> do
            let sop = StoreObjectPath.toText objPath
            let errMsg = "deriver for store object " <> sop <> " doesn't exist"
            throwError $ appError errMsg

    drv <- ensureDerivation drvPath (CapDerivation.loadDerivation storeDir)
    outName <- do
        let res = find (\(_, out) -> (out ^. #path == objPath)) (Map.toList (drv ^. #outputs))
        case res of
            Just (outName, _) -> pure outName
            Nothing -> do
                let dp = DerivingPath.toText drvPath
                let sop = StoreObjectPath.toText objPath
                let errMsg = "derivation " <> dp <> " doesn't output " <> sop
                throwError $ appError errMsg

    pure (drvPath, outName)

resolveDrvInputObjPaths
    :: (CapDerivation m)
    => FilePath -> Derivation -> CurrentT m (Map StoreObjectPath (DerivingPath, Text))
resolveDrvInputObjPaths storeDir drv = do
    let Derivation{inputDrvs} = drv

    inputs <- forM (Map.toList inputDrvs) $ \(inputDrvPath, outNames) -> do
        inputDrv <- ensureDerivation inputDrvPath (CapDerivation.loadDerivation storeDir)
        pure (inputDrvPath, inputDrv, outNames)

    drvObjPathsList <- forM inputs $ \(inputDrvPath, inputDrv, outNames) -> do
        let outs = inputDrv ^. #outputs
        forM (Set.toList outNames) $ \outName -> do
            case Map.lookup outName outs of
                Just DerivationOutput{path} -> pure (path, (inputDrvPath, outName))
                Nothing -> do
                    let dp = DerivingPath.toText inputDrvPath
                    let errMsg = "derivation " <> dp <> " doesn't output " <> outName
                    throwError $ appError errMsg

    pure $ Map.fromList (concat drvObjPathsList)

ensureDerivation
    :: (Monad m)
    => DerivingPath -> (DerivingPath -> AppExceptT m Derivation) -> CurrentT m Derivation
ensureDerivation drvPath loader = do
    res <- gets $ Map.lookup drvPath . (^. #loadedDrvCache)
    case res of
        Just drv -> pure drv
        Nothing -> do
            drv <- do
                let errMsg = "could not load derivation " <> DerivingPath.toText drvPath
                lift $ withErrContext errMsg $ loader drvPath
            modify $ #loadedDrvCache %~ Map.insert drvPath drv
            pure drv
