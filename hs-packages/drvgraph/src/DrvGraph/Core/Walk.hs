module DrvGraph.Core.Walk
    ( walk
    ) where

import Control.Monad (forM, forM_)
import Control.Monad.Except (throwError)
import Control.Monad.State.Strict (StateT (..), execStateT, get, modify)
import Control.Monad.Trans (lift)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)

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

-- | Traverse the Nix store from a derivation's output, which coresponds to a
-- store object path uniquely.
walk
    :: (CapDerivation m, CapStoreObject m)
    => FilePath -> DerivingPath -> Text -> AppExceptT m DepGraph
walk storeDir drvPath outName = do
    let initial = WalkState{wsLoadedDrvCache = Map.empty, wsDepGraph = DepGraph.empty}
    WalkState{wsDepGraph} <- execStateT (walkImpl storeDir drvPath outName) initial
    pure wsDepGraph

type CurrentT m = StateT WalkState (AppExceptT m)

data WalkState = WalkState
    { wsLoadedDrvCache :: Map DerivingPath Derivation
    , wsDepGraph :: DepGraph
    }

walkImpl
    :: (CapDerivation m, CapStoreObject m)
    => FilePath -> DerivingPath -> Text -> CurrentT m ()
walkImpl storeDir drvPath outName = do
    -- Get the @Derivation@ at @drvPath@.
    drv <- ensureDerivation drvPath (CapDerivation.loadDerivation storeDir)

    -- Insert @drv@'s @DrvNode@ to the dependency graph, if not done yet.
    depGraph <- wsDepGraph <$> get
    drvNode <- case DepGraph.lookupDrvNode drvPath depGraph of
        Just drvNode -> pure drvNode
        Nothing -> do
            drvInputObjPaths <- resolveDrvInputObjPaths storeDir drv
            let drvNode = DrvNode{drvInputObjPaths}
            modify $ withDepGraph $ DepGraph.insertDrvNode drvPath drvNode
            pure drvNode

    -- Get the @StoreObjectPath@ of @drvPath@^@outName@.
    stObjPath <- case Map.lookup outName . Derivation.drvOutputs $ drv of
        Just output -> do
            let DerivationOutput{outPath} = output
            pure outPath
        Nothing -> do
            let dp = DerivingPath.toText drvPath
            let errMsg = "derivation " <> dp <> " doesn't contain output " <> outName
            throwError $ appError errMsg

    -- Check whether we have visited the current store object path. If not,
    -- process it and recurse into its dependencies.
    maybeVisitedObj <- DepGraph.lookupObjNode stObjPath . wsDepGraph <$> get
    case maybeVisitedObj of
        Just _ -> pure ()
        Nothing -> resolveObjNodeAndRecurse storeDir drvPath stObjPath drvNode

resolveObjNodeAndRecurse
    :: (CapDerivation m, CapStoreObject m)
    => FilePath -> DerivingPath -> StoreObjectPath -> DrvNode -> CurrentT m ()
resolveObjNodeAndRecurse storeDir drvPath stObjPath drvNode = do
    isSynced <- do
        let errMsg = "could not query local store object path" <> StoreObjectPath.toText stObjPath
        lift $ withErrContext errMsg $ CapStoreObject.queryLocalStoreObject storeDir stObjPath

    if isSynced
        -- Stop at already existed store object path, whose its closure
        -- exists as well.
        then do
            modify $ withDepGraph $ DepGraph.insertObjNode stObjPath ObjExisted
        else do
            maybeNarInfo <- do
                let errMsg = "could not query remote store object path" <> StoreObjectPath.toText stObjPath
                lift $ withErrContext errMsg $ CapStoreObject.queryRemoteStoreObject stObjPath

            -- Get the list of dependent input derivation outputs to recurse into.
            let DrvNode{drvInputObjPaths} = drvNode
            inputDrvs <- case maybeNarInfo of
                Just NarInfo{narInfoRefs = stRefPaths} -> do
                    -- Store object in remote binary caches only needs runtime
                    -- dependencies included in its narinfo's @References@ line.
                    let objNode = ObjUnsynced{stDrvPath = drvPath, stRefPaths}
                    modify $ withDepGraph $ DepGraph.insertObjNode stObjPath objNode

                    forM (Set.toList stRefPaths) $ \stRefPath ->
                        case Map.lookup stRefPath drvInputObjPaths of
                            Just inputDrv -> pure inputDrv
                            Nothing -> do
                                let sop = StoreObjectPath.toText stRefPath
                                let errMsg = "deriver for store object " <> sop <> " doesn't exist"
                                throwError $ appError errMsg
                Nothing -> do
                    -- Otherwise Nix needs to build this derivation locally, so
                    -- all input derivation outputs are required.
                    let objNode = ObjUnbuilt{stDrvPath = drvPath}
                    modify $ withDepGraph $ DepGraph.insertObjNode stObjPath objNode

                    pure $ foldr (:) [] drvInputObjPaths

            -- Recurse into dependencies.
            forM_ inputDrvs $ \(inputDrv, inputDrvOutName) -> do
                walkImpl storeDir inputDrv inputDrvOutName

resolveDrvInputObjPaths
    :: (CapDerivation m)
    => FilePath -> Derivation -> CurrentT m (Map StoreObjectPath (DerivingPath, Text))
resolveDrvInputObjPaths storeDir drv = do
    let Derivation{drvInputDrvs} = drv

    inputs <- forM (Map.toList drvInputDrvs) $ \(inputDrvPath, outNames) -> do
        inputDrv <- ensureDerivation inputDrvPath (CapDerivation.loadDerivation storeDir)
        pure (inputDrvPath, inputDrv, outNames)

    drvObjPathsList <- forM inputs $ \(inputDrvPath, inputDrv, outNames) -> do
        let outs = drvOutputs inputDrv
        forM (Set.toList outNames) $ \outName -> do
            case Map.lookup outName outs of
                Just DerivationOutput{outPath} -> pure (outPath, (inputDrvPath, outName))
                Nothing -> do
                    let dp = DerivingPath.toText inputDrvPath
                    let errMsg = "derivation " <> dp <> " doesn't output " <> outName
                    throwError $ appError errMsg

    pure $ Map.fromList (concat drvObjPathsList)

ensureDerivation
    :: (Monad m)
    => DerivingPath -> (DerivingPath -> AppExceptT m Derivation) -> CurrentT m Derivation
ensureDerivation drvPath loader = do
    res <- Map.lookup drvPath . wsLoadedDrvCache <$> get
    case res of
        Just drv -> pure drv
        Nothing -> do
            drv <- do
                let errMsg = "could not load derivation: " <> DerivingPath.toText drvPath
                lift $ withErrContext errMsg $ loader drvPath
            modify $ withLoadedDrvCache $ Map.insert drvPath drv
            pure drv

withLoadedDrvCache
    :: (Map DerivingPath Derivation -> Map DerivingPath Derivation)
    -> WalkState
    -> WalkState
withLoadedDrvCache f state@WalkState{wsLoadedDrvCache} =
    state{wsLoadedDrvCache = f wsLoadedDrvCache}

withDepGraph
    :: (DepGraph -> DepGraph)
    -> WalkState
    -> WalkState
withDepGraph f state@WalkState{wsDepGraph} =
    state{wsDepGraph = f wsDepGraph}
