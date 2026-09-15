module DrvGraph.Core.WalkTest
    ( unit_walkUnsyncedThenLocalLeaf
    , unit_walkUnbuiltFallback
    , unit_walkUnsyncedDiamond
    , unit_walkMultiOutputs
    , unit_walkMissingOutputNameError
    , unit_walkUnsyncedRefWithoutDeriverError
    , unit_walkInputDrvMissingOutputNameError
    ) where

import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Data.Either (isLeft)
import Data.Function ((&))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Optics ((^.))
import Optics.TH (makeFieldLabelsNoPrefix)
import Test.Tasty.HUnit (assertBool, assertFailure, (@?=))

import DrvGraph.Core.Capability.CapDerivation (CapDerivation (loadDerivation))
import DrvGraph.Core.Capability.CapStoreObject (CapStoreObject (queryLocalStoreObject, queryRemoteStoreObject), NarInfo (..))
import DrvGraph.Core.Error (AppEither, renderAppError, throwAppErrorText, tryAppError)
import DrvGraph.Core.Model.DepGraph (DepGraph (..), DrvNode (..), ObjNode (..))
import DrvGraph.Core.Model.DepGraph qualified as DepGraph
import DrvGraph.Core.Model.Derivation (Derivation (..), DerivationOutput (..))
import DrvGraph.Core.Model.DerivingPath (DerivingPath)
import DrvGraph.Core.Model.DerivingPath qualified as DerivingPath
import DrvGraph.Core.Model.StoreObjectPath (StoreObjectPath)
import DrvGraph.Core.Model.StoreObjectPath qualified as StoreObjectPath
import DrvGraph.Core.Walk (walk)

data TestEnv = TestEnv
    { derivations :: Map DerivingPath Derivation
    , localObjects :: Set StoreObjectPath
    , remoteNars :: Map StoreObjectPath NarInfo
    }

makeFieldLabelsNoPrefix ''TestEnv

defaultEnv :: TestEnv
defaultEnv =
    TestEnv
        { derivations = Map.empty
        , localObjects = Set.empty
        , remoteNars = Map.empty
        }

instance CapDerivation (ReaderT TestEnv IO) where
    loadDerivation :: FilePath -> DerivingPath -> ReaderT TestEnv IO Derivation
    loadDerivation _storeDir drvPath = do
        derivations <- asks (^. #derivations)
        case Map.lookup drvPath derivations of
            Just drv -> pure drv
            Nothing -> throwAppErrorText "derivation not found in test environment"

instance CapStoreObject (ReaderT TestEnv IO) where
    queryLocalStoreObject :: FilePath -> StoreObjectPath -> ReaderT TestEnv IO Bool
    queryLocalStoreObject _storeDir stObjPath = do
        localObjects <- asks (^. #localObjects)
        pure $ Set.member stObjPath localObjects

    queryRemoteStoreObject :: StoreObjectPath -> ReaderT TestEnv IO (Maybe NarInfo)
    queryRemoteStoreObject stObjPath = do
        remoteNars <- asks (^. #remoteNars)
        pure $ Map.lookup stObjPath remoteNars

runWalk :: TestEnv -> DerivingPath -> Text -> IO (AppEither (DepGraph, StoreObjectPath))
runWalk env drvPath outName =
    runReaderT (tryAppError (walk "/nix/store" drvPath outName)) env

runWalkRight :: TestEnv -> DerivingPath -> Text -> IO (DepGraph, StoreObjectPath)
runWalkRight env drvPath outName =
    runWalk env drvPath outName >>= either (assertFailure . Text.unpack . renderAppError) pure

fakeHash :: Int -> Text
fakeHash i = Text.replicate 32 (Text.singleton (alphaNums !! i))
  where
    alphaNums = "0123456789abcdfghijklmnpqrsvwxyz"

drvPathOf :: Int -> Text -> DerivingPath
drvPathOf n name = DerivingPath.uncheckedText (fakeHash n <> "-" <> name <> ".drv")

storeObjectPathOf :: Int -> Text -> StoreObjectPath
storeObjectPathOf n name = StoreObjectPath.uncheckedText (fakeHash n <> "-" <> name)

drvPathA, drvPathB, drvPathC, drvPathD :: DerivingPath
drvPathA = drvPathOf 0 "a"
drvPathB = drvPathOf 1 "b"
drvPathC = drvPathOf 2 "c"
drvPathD = drvPathOf 3 "d"

objPathA, objPathB, objPathBDev, objPathC, objPathD, objPathX :: StoreObjectPath
objPathA = storeObjectPathOf 4 "a"
objPathB = storeObjectPathOf 5 "b"
objPathBDev = storeObjectPathOf 6 "b-dev"
objPathC = storeObjectPathOf 7 "c"
objPathD = storeObjectPathOf 8 "d"
objPathX = storeObjectPathOf 9 "x"

mkInputs :: [(DerivingPath, Text)] -> Map DerivingPath (Set Text)
mkInputs = foldl' (\mp (dp, out) -> Map.insertWith Set.union dp (Set.singleton out) mp) Map.empty

mkDerivation
    :: Map DerivingPath (Set Text)
    -> Map Text StoreObjectPath
    -> Derivation
mkDerivation inputDrvs outputPaths =
    Derivation
        { inputDrvs
        , inputSrcs = Set.empty
        , outputs = Map.map toDerivationOutput outputPaths
        , platform = "x86_64-linux"
        , builder = "/nix/store/0bash/bin/bash"
        , args = []
        , envs = Map.empty
        }
  where
    toDerivationOutput path = DerivationOutput{path, hash = Nothing}

applyObjNodeInsertions :: [(StoreObjectPath, ObjNode)] -> DepGraph -> DepGraph
applyObjNodeInsertions pairs graph = foldr (uncurry DepGraph.insertObjNode) graph pairs

applyDrvNodeInsertions :: [(DerivingPath, DrvNode)] -> DepGraph -> DepGraph
applyDrvNodeInsertions pairs graph = foldr (uncurry DepGraph.insertDrvNode) graph pairs

unit_walkUnsyncedThenLocalLeaf :: IO ()
unit_walkUnsyncedThenLocalLeaf = do
    let drvA = mkDerivation (mkInputs [(drvPathB, "out")]) (Map.fromList [("out", objPathA)])
    let drvB = mkDerivation Map.empty (Map.fromList [("out", objPathB)])
    let narA = NarInfo{narInfoRefs = Set.fromList [objPathB]}
    let env =
            defaultEnv
                { derivations = Map.fromList [(drvPathA, drvA), (drvPathB, drvB)]
                , localObjects = Set.singleton objPathB
                , remoteNars = Map.fromList [(objPathA, narA)]
                }

    (depGraph, objPath) <- runWalkRight env drvPathA "out"

    depGraph
        @?= ( DepGraph.empty
                & applyObjNodeInsertions
                    [ (objPathA, ObjUnsynced{refPaths = Set.fromList [objPathB]})
                    , (objPathB, ObjExisted)
                    ]
            )
    objPath @?= objPathA

unit_walkUnbuiltFallback :: IO ()
unit_walkUnbuiltFallback = do
    let drvA = mkDerivation (mkInputs [(drvPathB, "out")]) (Map.fromList [("out", objPathA)])
    let drvB = mkDerivation Map.empty (Map.fromList [("out", objPathB)])
    let env =
            defaultEnv
                { derivations = Map.fromList [(drvPathA, drvA), (drvPathB, drvB)]
                }

    (depGraph, objPath) <- runWalkRight env drvPathA "out"

    depGraph
        @?= ( DepGraph.empty
                & applyObjNodeInsertions
                    [ (objPathA, ObjUnbuilt{drvPath = drvPathA})
                    , (objPathB, ObjUnbuilt{drvPath = drvPathB})
                    ]
                & applyDrvNodeInsertions
                    [ (drvPathA, DrvNode{inputObjPaths = Set.fromList [objPathB]})
                    , (drvPathB, DrvNode{inputObjPaths = Set.empty})
                    ]
            )
    objPath @?= objPathA

unit_walkUnsyncedDiamond :: IO ()
unit_walkUnsyncedDiamond = do
    let drvA = mkDerivation (mkInputs [(drvPathB, "out"), (drvPathC, "out")]) (Map.fromList [("out", objPathA)])
    let drvB = mkDerivation (mkInputs [(drvPathD, "out")]) (Map.fromList [("out", objPathB)])
    let drvC = mkDerivation (mkInputs [(drvPathD, "out")]) (Map.fromList [("out", objPathC)])
    let drvD = mkDerivation Map.empty (Map.fromList [("out", objPathD)])
    let narA = NarInfo{narInfoRefs = Set.fromList [objPathB, objPathC]}
    let narB = NarInfo{narInfoRefs = Set.fromList [objPathD]}
    let narC = NarInfo{narInfoRefs = Set.fromList [objPathD]}
    let narD = NarInfo{narInfoRefs = Set.fromList []}
    let env =
            defaultEnv
                { derivations = Map.fromList [(drvPathA, drvA), (drvPathB, drvB), (drvPathC, drvC), (drvPathD, drvD)]
                , remoteNars = Map.fromList [(objPathA, narA), (objPathB, narB), (objPathC, narC), (objPathD, narD)]
                }

    (depGraph, objPath) <- runWalkRight env drvPathA "out"

    depGraph
        @?= ( DepGraph.empty
                & applyObjNodeInsertions
                    [ (objPathA, ObjUnsynced{refPaths = Set.fromList [objPathB, objPathC]})
                    , (objPathB, ObjUnsynced{refPaths = Set.fromList [objPathD]})
                    , (objPathC, ObjUnsynced{refPaths = Set.fromList [objPathD]})
                    , (objPathD, ObjUnsynced{refPaths = Set.empty})
                    ]
            )
    objPath @?= objPathA

unit_walkMultiOutputs :: IO ()
unit_walkMultiOutputs = do
    let drvA = mkDerivation (mkInputs [(drvPathB, "out"), (drvPathB, "dev")]) (Map.fromList [("out", objPathA)])
    let drvB = mkDerivation (mkInputs [(drvPathC, "out")]) (Map.fromList [("out", objPathB), ("dev", objPathBDev)])
    let drvC = mkDerivation (mkInputs []) (Map.fromList [("out", objPathC)])
    let narC = NarInfo{narInfoRefs = Set.empty}
    let env =
            defaultEnv
                { derivations = Map.fromList [(drvPathA, drvA), (drvPathB, drvB), (drvPathC, drvC)]
                , remoteNars = Map.fromList [(objPathC, narC)]
                }

    (depGraph, objPath) <- runWalkRight env drvPathA "out"

    depGraph
        @?= ( DepGraph.empty
                & applyObjNodeInsertions
                    [ (objPathA, ObjUnbuilt{drvPath = drvPathA})
                    , (objPathB, ObjUnbuilt{drvPath = drvPathB})
                    , (objPathBDev, ObjUnbuilt{drvPath = drvPathB})
                    , (objPathC, ObjUnsynced{refPaths = Set.empty})
                    ]
                & applyDrvNodeInsertions
                    [ (drvPathA, DrvNode{inputObjPaths = Set.fromList [objPathB, objPathBDev]})
                    , (drvPathB, DrvNode{inputObjPaths = Set.fromList [objPathC]})
                    ]
            )
    objPath @?= objPathA

unit_walkMissingOutputNameError :: IO ()
unit_walkMissingOutputNameError = do
    let drvA = mkDerivation Map.empty (Map.fromList [("out", objPathA)])
    let env =
            defaultEnv
                { derivations = Map.fromList [(drvPathA, drvA)]
                }

    actual <- runWalk env drvPathA "dev"

    assertBool "expected a Left error" (isLeft actual)

unit_walkUnsyncedRefWithoutDeriverError :: IO ()
unit_walkUnsyncedRefWithoutDeriverError = do
    let drvA = mkDerivation Map.empty (Map.fromList [("out", objPathA)])
    let narA = NarInfo{narInfoRefs = Set.fromList [objPathX]}
    let env =
            defaultEnv
                { derivations = Map.fromList [(drvPathA, drvA)]
                , remoteNars = Map.fromList [(objPathA, narA)]
                }

    actual <- runWalk env drvPathA "out"

    assertBool "expected a Left error" (isLeft actual)

unit_walkInputDrvMissingOutputNameError :: IO ()
unit_walkInputDrvMissingOutputNameError = do
    let drvA = mkDerivation (mkInputs [(drvPathB, "dev")]) (Map.fromList [("out", objPathA)])
    let drvB = mkDerivation Map.empty (Map.fromList [("out", objPathB)])
    let env =
            defaultEnv
                { derivations = Map.fromList [(drvPathA, drvA), (drvPathB, drvB)]
                }

    actual <- runWalk env drvPathA "out"

    assertBool "expected a Left error" (isLeft actual)
