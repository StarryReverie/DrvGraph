module DrvGraph.Core.TreeRepresentationTest
    ( unit_toDisplayTreeAllBranches
    , unit_toDisplayTreeUnbuiltWithDrvLeaf
    ) where

import Data.Function ((&))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Test.Tasty.HUnit ((@?=))

import DrvGraph.Core.Model.DepGraph (DepGraph, DrvNode (..), ObjNode (..))
import DrvGraph.Core.Model.DepGraph qualified as DepGraph
import DrvGraph.Core.Model.DerivingPath (DerivingPath)
import DrvGraph.Core.Model.DerivingPath qualified as DerivingPath
import DrvGraph.Core.Model.StoreObjectPath (StoreObjectPath)
import DrvGraph.Core.Model.StoreObjectPath qualified as StoreObjectPath
import DrvGraph.Core.TreeRepresentation (DerivationTree (..), StoreObjectTree (..), TreeRepresentationOptions (..), depGraphToTreeRepresentation)

fakeHash :: Int -> Text
fakeHash i = Text.replicate 32 (Text.singleton (alphaNums !! i))
  where
    alphaNums = "0123456789abcdfghijklmnpqrsvwxyz"

drvPathOf :: Int -> Text -> DerivingPath
drvPathOf n name = DerivingPath.uncheckedText (fakeHash n <> "-" <> name <> ".drv")

storeObjectPathOf :: Int -> Text -> StoreObjectPath
storeObjectPathOf n name = StoreObjectPath.uncheckedText (fakeHash n <> "-" <> name)

objRoot, objA, objB, objC, objD, objE, objG, objR, objO :: StoreObjectPath
objRoot = storeObjectPathOf 0 "root"
objA = storeObjectPathOf 1 "a"
objB = storeObjectPathOf 2 "b"
objC = storeObjectPathOf 3 "c"
objD = storeObjectPathOf 4 "d"
objE = storeObjectPathOf 5 "e"
objG = storeObjectPathOf 6 "g"
objR = storeObjectPathOf 7 "r"
objO = storeObjectPathOf 8 "o"

drvRoot, drvA, drvShared, drvO :: DerivingPath
drvRoot = drvPathOf 0 "root"
drvA = drvPathOf 1 "a"
drvShared = drvPathOf 2 "shared"
drvO = drvPathOf 6 "o"

applyObjNodeInsertions :: [(StoreObjectPath, ObjNode)] -> DepGraph -> DepGraph
applyObjNodeInsertions pairs graph = foldr (uncurry DepGraph.insertObjNode) graph pairs

applyDrvNodeInsertions :: [(DerivingPath, DrvNode)] -> DepGraph -> DepGraph
applyDrvNodeInsertions pairs graph = foldr (uncurry DepGraph.insertDrvNode) graph pairs

testDefaultOptions :: TreeRepresentationOptions
testDefaultOptions =
    TreeRepresentationOptions
        { includeExisted = True
        , includeVisited = True
        , maxDepth = Nothing
        }

unit_toDisplayTreeAllBranches :: IO ()
unit_toDisplayTreeAllBranches = do
    let graph =
            DepGraph.empty
                & applyObjNodeInsertions
                    [ (objRoot, ObjUnbuilt{drvPath = drvRoot})
                    , (objA, ObjUnsynced{refPaths = Set.singleton objC})
                    , (objB, ObjUnbuilt{drvPath = drvShared})
                    , (objC, ObjExisted)
                    , (objD, ObjUnbuilt{drvPath = drvShared})
                    , (objE, ObjUnsynced{refPaths = Set.empty})
                    , (objG, ObjExisted)
                    ]
                & applyDrvNodeInsertions
                    [ (drvRoot, DrvNode{inputObjPaths = Set.fromList [objA, objB, objD, objE]})
                    , (drvA, DrvNode{inputObjPaths = Set.empty})
                    , (drvShared, DrvNode{inputObjPaths = Set.fromList [objC, objG]})
                    ]

    let Just actual = depGraphToTreeRepresentation testDefaultOptions graph objRoot

    actual
        @?= StObjTreeUnbuiltWithDrv
            { objPath = objRoot
            , drvPath = drvRoot
            , objChildren =
                [ StObjTreeUnsynced
                    { objPath = objA
                    , refChildren = [StObjTreeExisted{objPath = objC}]
                    }
                , StObjTreeUnbuilt
                    { objPath = objB
                    , drvChild =
                        DrvTreeUnbuilt
                            { drvPath = drvShared
                            , objChildren =
                                [ StObjTreeVisited{objPath = objC}
                                , StObjTreeExisted{objPath = objG}
                                ]
                            }
                    }
                , StObjTreeUnbuilt
                    { objPath = objD
                    , drvChild = DrvTreeVisited{drvPath = drvShared}
                    }
                , StObjTreeUnsynced
                    { objPath = objE
                    , refChildren = []
                    }
                ]
            }

unit_toDisplayTreeUnbuiltWithDrvLeaf :: IO ()
unit_toDisplayTreeUnbuiltWithDrvLeaf = do
    let graph =
            DepGraph.empty
                & applyObjNodeInsertions
                    [ (objR, ObjUnsynced{refPaths = Set.singleton objO})
                    , (objO, ObjUnbuilt{drvPath = drvO})
                    ]
                & applyDrvNodeInsertions
                    [ (drvO, DrvNode{inputObjPaths = Set.empty})
                    ]

    let Just actual = depGraphToTreeRepresentation testDefaultOptions graph objR

    actual
        @?= StObjTreeUnsynced
            { objPath = objR
            , refChildren =
                [ StObjTreeUnbuiltWithDrv
                    { objPath = objO
                    , drvPath = drvO
                    , objChildren = []
                    }
                ]
            }
