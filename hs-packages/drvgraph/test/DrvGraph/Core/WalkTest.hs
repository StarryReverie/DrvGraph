module DrvGraph.Core.WalkTest
    ( unit_walkLocalExisted
    , unit_walkUnsyncedThenLocalLeaf
    , unit_walkUnbuiltFallback
    , unit_walkDiamondVisitsOnce
    , unit_walkMissingOutputNameError
    , unit_walkUnsyncedRefWithoutDeriverError
    , unit_walkInputDrvMissingOutputNameError
    ) where

import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.Reader (Reader, asks, runReader)
import Data.Either (isLeft)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Test.Tasty.HUnit (assertBool, (@?=))

import DrvGraph.Core.Capability.CapDerivation (CapDerivation (loadDerivation))
import DrvGraph.Core.Capability.CapStoreObject (CapStoreObject (..), NarInfo (..))
import DrvGraph.Core.Error (AppEither, AppExceptT, appError)
import DrvGraph.Core.Model.DepGraph (DepGraph (..), DrvNode (..), ObjNode (..))
import DrvGraph.Core.Model.Derivation (Derivation (..), DerivationOutput (..))
import DrvGraph.Core.Model.DerivingPath (DerivingPath)
import DrvGraph.Core.Model.DerivingPath qualified as DerivingPath
import DrvGraph.Core.Model.StoreObjectPath (StoreObjectPath)
import DrvGraph.Core.Model.StoreObjectPath qualified as StoreObjectPath
import DrvGraph.Core.Walk (walk)

data TestEnv = TestEnv
    { envDerivations :: Map DerivingPath Derivation
    , envLocalObjects :: Set StoreObjectPath
    , envRemoteNars :: Map StoreObjectPath NarInfo
    }

defaultEnv :: TestEnv
defaultEnv =
    TestEnv
        { envDerivations = Map.empty
        , envLocalObjects = Set.empty
        , envRemoteNars = Map.empty
        }

instance CapDerivation (Reader TestEnv) where
    loadDerivation :: FilePath -> DerivingPath -> AppExceptT (Reader TestEnv) Derivation
    loadDerivation _storeDir drvPath = do
        derivations <- asks envDerivations
        case Map.lookup drvPath derivations of
            Just drv -> pure drv
            Nothing -> throwError $ appError "derivation not found in test environment"

instance CapStoreObject (Reader TestEnv) where
    queryLocalStoreObject :: FilePath -> StoreObjectPath -> AppExceptT (Reader TestEnv) Bool
    queryLocalStoreObject _storeDir stObjPath = do
        localObjects <- asks envLocalObjects
        pure $ Set.member stObjPath localObjects

    queryRemoteStoreObject :: StoreObjectPath -> AppExceptT (Reader TestEnv) (Maybe NarInfo)
    queryRemoteStoreObject stObjPath = do
        remoteNars <- asks envRemoteNars
        pure $ Map.lookup stObjPath remoteNars

runWalk :: TestEnv -> DerivingPath -> Text -> AppEither DepGraph
runWalk env drvPath outName =
    runReader (runExceptT (walk "/nix/store" drvPath outName)) env

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

objPathA, objPathB, objPathC, objPathD, objPathX :: StoreObjectPath
objPathA = storeObjectPathOf 4 "a"
objPathB = storeObjectPathOf 5 "b"
objPathC = storeObjectPathOf 6 "c"
objPathD = storeObjectPathOf 7 "d"
objPathX = storeObjectPathOf 8 "x"

mkInputs :: [(DerivingPath, Text)] -> Map DerivingPath (Set Text)
mkInputs = Map.fromList . fmap (fmap Set.singleton)

mkDerivation
    :: Map DerivingPath (Set Text)
    -> Map Text StoreObjectPath
    -> Derivation
mkDerivation drvInputDrvs outputPaths =
    Derivation
        { drvInputDrvs
        , drvInputSrcs = Set.empty
        , drvOutputs = Map.map toDerivationOutput outputPaths
        , drvPlatform = "x86_64-linux"
        , drvBuilder = "/nix/store/0bash/bin/bash"
        , drvArgs = []
        , drvEnvs = Map.empty
        }
  where
    toDerivationOutput outPath = DerivationOutput{outPath, outHash = Nothing}

unit_walkLocalExisted :: IO ()
unit_walkLocalExisted = do
    let drvA = mkDerivation Map.empty (Map.fromList [("out", objPathA)])
    let env =
            defaultEnv
                { envDerivations = Map.fromList [(drvPathA, drvA)]
                , envLocalObjects = Set.singleton objPathA
                }

    let Right actual = runWalk env drvPathA "out"

    actual
        @?= DepGraph
            { dgObjNodes =
                Map.fromList
                    [ (objPathA, ObjExisted{stObjPath = objPathA})
                    ]
            , dgDrvNodes =
                Map.fromList
                    [ (drvPathA, DrvNode{drvPath = drvPathA, drvInputObjPaths = Map.empty})
                    ]
            }

unit_walkUnsyncedThenLocalLeaf :: IO ()
unit_walkUnsyncedThenLocalLeaf = do
    let drvA = mkDerivation (mkInputs [(drvPathB, "out")]) (Map.fromList [("out", objPathA)])
    let drvB = mkDerivation Map.empty (Map.fromList [("out", objPathB)])
    let narA = NarInfo{narInfoRefs = Set.fromList [objPathB]}
    let env =
            defaultEnv
                { envDerivations = Map.fromList [(drvPathA, drvA), (drvPathB, drvB)]
                , envLocalObjects = Set.singleton objPathB
                , envRemoteNars = Map.fromList [(objPathA, narA)]
                }

    let Right actual = runWalk env drvPathA "out"

    actual
        @?= DepGraph
            { dgObjNodes =
                Map.fromList
                    [ (objPathA, ObjUnsynced{stObjPath = objPathA, stDrvPath = drvPathA, stRefPaths = Set.fromList [objPathB]})
                    , (objPathB, ObjExisted{stObjPath = objPathB})
                    ]
            , dgDrvNodes =
                Map.fromList
                    [ (drvPathA, DrvNode{drvPath = drvPathA, drvInputObjPaths = Map.fromList [(objPathB, (drvPathB, "out"))]})
                    , (drvPathB, DrvNode{drvPath = drvPathB, drvInputObjPaths = Map.empty})
                    ]
            }

unit_walkUnbuiltFallback :: IO ()
unit_walkUnbuiltFallback = do
    let drvA = mkDerivation (mkInputs [(drvPathB, "out")]) (Map.fromList [("out", objPathA)])
    let drvB = mkDerivation Map.empty (Map.fromList [("out", objPathB)])
    let env =
            defaultEnv
                { envDerivations = Map.fromList [(drvPathA, drvA), (drvPathB, drvB)]
                }

    let Right actual = runWalk env drvPathA "out"

    actual
        @?= DepGraph
            { dgObjNodes =
                Map.fromList
                    [ (objPathA, ObjUnbuilt{stObjPath = objPathA, stDrvPath = drvPathA})
                    , (objPathB, ObjUnbuilt{stObjPath = objPathB, stDrvPath = drvPathB})
                    ]
            , dgDrvNodes =
                Map.fromList
                    [ (drvPathA, DrvNode{drvPath = drvPathA, drvInputObjPaths = Map.fromList [(objPathB, (drvPathB, "out"))]})
                    , (drvPathB, DrvNode{drvPath = drvPathB, drvInputObjPaths = Map.empty})
                    ]
            }

unit_walkDiamondVisitsOnce :: IO ()
unit_walkDiamondVisitsOnce = do
    let drvA = mkDerivation (mkInputs [(drvPathB, "out"), (drvPathC, "out")]) (Map.fromList [("out", objPathA)])
    let drvB = mkDerivation (mkInputs [(drvPathD, "out")]) (Map.fromList [("out", objPathB)])
    let drvC = mkDerivation (mkInputs [(drvPathD, "out")]) (Map.fromList [("out", objPathC)])
    let drvD = mkDerivation Map.empty (Map.fromList [("out", objPathD)])
    let narA = NarInfo{narInfoRefs = Set.fromList [objPathB, objPathC]}
    let narB = NarInfo{narInfoRefs = Set.fromList [objPathD]}
    let narC = NarInfo{narInfoRefs = Set.fromList [objPathD]}
    let env =
            defaultEnv
                { envDerivations = Map.fromList [(drvPathA, drvA), (drvPathB, drvB), (drvPathC, drvC), (drvPathD, drvD)]
                , envRemoteNars = Map.fromList [(objPathA, narA), (objPathB, narB), (objPathC, narC)]
                }

    let Right actual = runWalk env drvPathA "out"

    actual
        @?= DepGraph
            { dgObjNodes =
                Map.fromList
                    [ (objPathA, ObjUnsynced{stObjPath = objPathA, stDrvPath = drvPathA, stRefPaths = Set.fromList [objPathB, objPathC]})
                    , (objPathB, ObjUnsynced{stObjPath = objPathB, stDrvPath = drvPathB, stRefPaths = Set.fromList [objPathD]})
                    , (objPathC, ObjUnsynced{stObjPath = objPathC, stDrvPath = drvPathC, stRefPaths = Set.fromList [objPathD]})
                    , (objPathD, ObjUnbuilt{stObjPath = objPathD, stDrvPath = drvPathD})
                    ]
            , dgDrvNodes =
                Map.fromList
                    [ (drvPathA, DrvNode{drvPath = drvPathA, drvInputObjPaths = Map.fromList [(objPathB, (drvPathB, "out")), (objPathC, (drvPathC, "out"))]})
                    , (drvPathB, DrvNode{drvPath = drvPathB, drvInputObjPaths = Map.fromList [(objPathD, (drvPathD, "out"))]})
                    , (drvPathC, DrvNode{drvPath = drvPathC, drvInputObjPaths = Map.fromList [(objPathD, (drvPathD, "out"))]})
                    , (drvPathD, DrvNode{drvPath = drvPathD, drvInputObjPaths = Map.empty})
                    ]
            }

unit_walkMissingOutputNameError :: IO ()
unit_walkMissingOutputNameError = do
    let drvA = mkDerivation Map.empty (Map.fromList [("out", objPathA)])
    let env =
            defaultEnv
                { envDerivations = Map.fromList [(drvPathA, drvA)]
                }

    let actual = runWalk env drvPathA "dev"

    assertBool "expected a Left error" (isLeft actual)

unit_walkUnsyncedRefWithoutDeriverError :: IO ()
unit_walkUnsyncedRefWithoutDeriverError = do
    let drvA = mkDerivation Map.empty (Map.fromList [("out", objPathA)])
    let narA = NarInfo{narInfoRefs = Set.fromList [objPathX]}
    let env =
            defaultEnv
                { envDerivations = Map.fromList [(drvPathA, drvA)]
                , envRemoteNars = Map.fromList [(objPathA, narA)]
                }

    let actual = runWalk env drvPathA "out"

    assertBool "expected a Left error" (isLeft actual)

unit_walkInputDrvMissingOutputNameError :: IO ()
unit_walkInputDrvMissingOutputNameError = do
    let drvA = mkDerivation (mkInputs [(drvPathB, "dev")]) (Map.fromList [("out", objPathA)])
    let drvB = mkDerivation Map.empty (Map.fromList [("out", objPathB)])
    let env =
            defaultEnv
                { envDerivations = Map.fromList [(drvPathA, drvA), (drvPathB, drvB)]
                }

    let actual = runWalk env drvPathA "out"

    assertBool "expected a Left error" (isLeft actual)
