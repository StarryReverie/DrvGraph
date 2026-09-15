module DrvGraph.Core.PrettyPrint
    ( EntryLine (..)
    , EntryLineColor (..)
    , treeToLines
    ) where

import Data.DList (DList)
import Data.DList qualified as DList
import Data.Text (Text)
import Optics ((^.))
import Optics.TH (makeFieldLabelsNoPrefix)
import System.Console.ANSI (Color (Blue, Green, Magenta, White, Yellow), ColorIntensity (Dull, Vivid))

import DrvGraph.Core.Model.DerivingPath (DerivingPath)
import DrvGraph.Core.Model.DerivingPath qualified as DerivingPath
import DrvGraph.Core.Model.StoreObjectPath (StoreObjectPath)
import DrvGraph.Core.Model.StoreObjectPath qualified as StoreObjectPath
import DrvGraph.Core.TreeRepresentation (DerivationTree (..), StoreObjectTree (..))

data EntryLine = EntryLine
    { isSubtreeLastChild :: [Bool]
    , content :: [(Text, EntryLineColor)]
    }
    deriving (Eq, Show)

data EntryLineColor = EntryLineColor
    { color :: Color
    , intensity :: ColorIntensity
    }
    deriving (Eq, Show)

makeFieldLabelsNoPrefix ''EntryLine
makeFieldLabelsNoPrefix ''EntryLineColor

treeToLines :: StoreObjectTree -> [EntryLine]
treeToLines = DList.toList . objTreeToLines []

objTreeToLines :: [Bool] -> StoreObjectTree -> DList EntryLine
objTreeToLines pos StObjTreeExisted{objPath} =
    DList.singleton
        EntryLine
            { isSubtreeLastChild = pos
            , content =
                [ ("Existed", EntryLineColor{color = Green, intensity = Vivid})
                , makePackageNameTextChunk (objPath ^. #name)
                , makeStoreObjectPathTextChunk objPath
                ]
            }
objTreeToLines pos StObjTreeUnsynced{objPath, refChildren} =
    DList.cons
        ( EntryLine
            { isSubtreeLastChild = pos
            , content =
                [ ("Unsynced", EntryLineColor{color = Blue, intensity = Vivid})
                , makePackageNameTextChunk (objPath ^. #name)
                , makeStoreObjectPathTextChunk objPath
                ]
            }
        )
        (collectChildrenLines pos refChildren)
objTreeToLines pos StObjTreeUnbuilt{objPath, drvChild} =
    DList.cons
        ( EntryLine
            { isSubtreeLastChild = pos
            , content =
                [ ("Unbuilt", EntryLineColor{color = Yellow, intensity = Vivid})
                , makePackageNameTextChunk (objPath ^. #name)
                , makeStoreObjectPathTextChunk objPath
                ]
            }
        )
        (drvTreeToLines (True : pos) drvChild)
objTreeToLines pos StObjTreeUnbuiltWithDrv{objPath, drvPath, objChildren} =
    DList.cons
        ( EntryLine
            { isSubtreeLastChild = pos
            , content =
                [ ("Unbuilt", EntryLineColor{color = Yellow, intensity = Vivid})
                , makePackageNameTextChunk (objPath ^. #name)
                , makeStoreObjectPathTextChunk objPath
                , makeDerivingPathTextChunk drvPath
                ]
            }
        )
        (collectChildrenLines pos objChildren)
objTreeToLines pos StObjTreeVisited{objPath} =
    DList.singleton
        EntryLine
            { isSubtreeLastChild = pos
            , content =
                [ ("Visited", EntryLineColor{color = Magenta, intensity = Vivid})
                , makePackageNameTextChunk (objPath ^. #name)
                , makeStoreObjectPathTextChunk objPath
                ]
            }

drvTreeToLines :: [Bool] -> DerivationTree -> DList EntryLine
drvTreeToLines pos DrvTreeUnbuilt{drvPath, objChildren} =
    DList.cons
        ( EntryLine
            { isSubtreeLastChild = pos
            , content =
                [ ("Unbuilt", EntryLineColor{color = Yellow, intensity = Vivid})
                , makeDerivingPathTextChunk drvPath
                ]
            }
        )
        (collectChildrenLines pos objChildren)
drvTreeToLines pos DrvTreeVisited{drvPath} =
    DList.singleton
        EntryLine
            { isSubtreeLastChild = pos
            , content =
                [ ("Omitted", EntryLineColor{color = Magenta, intensity = Vivid})
                , makeDerivingPathTextChunk drvPath
                ]
            }

collectChildrenLines :: [Bool] -> [StoreObjectTree] -> DList EntryLine
collectChildrenLines pos =
    lastAwaredFoldr
        ((<>) . (\(child, isLast) -> objTreeToLines (isLast : pos) child))
        DList.empty

lastAwaredFoldr :: ((a, Bool) -> b -> b) -> b -> [a] -> b
lastAwaredFoldr _ initial [] = initial
lastAwaredFoldr f initial [x] = f (x, True) (lastAwaredFoldr f initial [])
lastAwaredFoldr f initial (x : xs) = f (x, False) (lastAwaredFoldr f initial xs)

makePackageNameTextChunk :: Text -> (Text, EntryLineColor)
makePackageNameTextChunk pkgName =
    ( pkgName
    , EntryLineColor{color = White, intensity = Vivid}
    )

makeStoreObjectPathTextChunk :: StoreObjectPath -> (Text, EntryLineColor)
makeStoreObjectPathTextChunk objPath =
    ( StoreObjectPath.toText objPath
    , EntryLineColor{color = Blue, intensity = Dull}
    )

makeDerivingPathTextChunk :: DerivingPath -> (Text, EntryLineColor)
makeDerivingPathTextChunk drvPath =
    ( DerivingPath.toText drvPath
    , EntryLineColor{color = Yellow, intensity = Dull}
    )
