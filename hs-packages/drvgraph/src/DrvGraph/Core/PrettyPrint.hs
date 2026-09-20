module DrvGraph.Core.PrettyPrint
    ( EntryLine (..)
    , EntryLineColor (..)
    , ToLinesOptions (..)
    , treeToLines
    ) where

import Data.DList (DList)
import Data.DList qualified as DList
import Data.Text (Text)
import Data.Text qualified as Text
import Optics ((^.))
import Optics.TH (makeFieldLabelsNoPrefix)
import System.Console.ANSI (Color (Blue, Cyan, Green, Magenta, White, Yellow), ColorIntensity (Dull, Vivid))

import DrvGraph.Core.Model.DerivingPath (DerivingPath)
import DrvGraph.Core.Model.DerivingPath qualified as DerivingPath
import DrvGraph.Core.Model.Nix32Hash qualified as Nix32Hash
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

data ToLinesOptions = ToLinesOptions
    { showFile :: Bool
    , reversed :: Bool
    }
    deriving (Eq, Show)

makeFieldLabelsNoPrefix ''EntryLine
makeFieldLabelsNoPrefix ''EntryLineColor
makeFieldLabelsNoPrefix ''ToLinesOptions

treeToLines :: ToLinesOptions -> StoreObjectTree -> [EntryLine]
treeToLines opts = reverseCond . DList.toList . objTreeToLines opts []
  where
    reverseCond = if opts ^. #reversed then reverse else id

objTreeToLines :: ToLinesOptions -> [Bool] -> StoreObjectTree -> DList EntryLine
objTreeToLines opts pos StObjTreeExisted{objPath} =
    DList.singleton
        EntryLine
            { isSubtreeLastChild = pos
            , content =
                [("Existed", EntryLineColor{color = Green, intensity = Vivid})]
                    <> makePackageNameTextChunks (objPath ^. #name)
                    <> makeStoreObjectPathTextChunks (opts ^. #showFile) objPath
            }
objTreeToLines opts pos StObjTreeUnsynced{objPath, refChildren} =
    DList.cons
        ( EntryLine
            { isSubtreeLastChild = pos
            , content =
                [("Unsynced", EntryLineColor{color = Blue, intensity = Vivid})]
                    <> makePackageNameTextChunks (objPath ^. #name)
                    <> makeStoreObjectPathTextChunks (opts ^. #showFile) objPath
            }
        )
        (collectChildrenLines opts pos refChildren)
objTreeToLines opts pos StObjTreeUnbuilt{objPath, drvChild} =
    DList.cons
        ( EntryLine
            { isSubtreeLastChild = pos
            , content =
                [("Unbuilt", EntryLineColor{color = Yellow, intensity = Vivid})]
                    <> makePackageNameTextChunks (objPath ^. #name)
                    <> makeStoreObjectPathTextChunks (opts ^. #showFile) objPath
            }
        )
        (drvTreeToLines opts (True : pos) drvChild)
objTreeToLines opts pos StObjTreeUnbuiltWithDrv{objPath, drvPath, objChildren} =
    DList.cons
        ( EntryLine
            { isSubtreeLastChild = pos
            , content =
                [("Unbuilt", EntryLineColor{color = Yellow, intensity = Vivid})]
                    <> makePackageNameTextChunks (objPath ^. #name)
                    <> makeStoreObjectPathTextChunks (opts ^. #showFile) objPath
                    <> makeDerivingPathTextChunks (opts ^. #showFile) drvPath
            }
        )
        (collectChildrenLines opts pos objChildren)
objTreeToLines opts pos StObjTreeVisited{objPath} =
    DList.singleton
        EntryLine
            { isSubtreeLastChild = pos
            , content =
                [("Visited", EntryLineColor{color = Magenta, intensity = Vivid})]
                    <> makePackageNameTextChunks (objPath ^. #name)
                    <> makeStoreObjectPathTextChunks (opts ^. #showFile) objPath
            }

drvTreeToLines :: ToLinesOptions -> [Bool] -> DerivationTree -> DList EntryLine
drvTreeToLines opts pos DrvTreeUnbuilt{drvPath, objChildren} =
    DList.cons
        ( EntryLine
            { isSubtreeLastChild = pos
            , content =
                [("Unbuilt", EntryLineColor{color = Yellow, intensity = Vivid})]
                    <> makeDerivingPathTextChunks True drvPath
            }
        )
        (collectChildrenLines opts pos objChildren)
drvTreeToLines _ pos DrvTreeVisited{drvPath} =
    DList.singleton
        EntryLine
            { isSubtreeLastChild = pos
            , content =
                [("Visited", EntryLineColor{color = Magenta, intensity = Vivid})]
                    <> makeDerivingPathTextChunks True drvPath
            }

collectChildrenLines :: ToLinesOptions -> [Bool] -> [StoreObjectTree] -> DList EntryLine
collectChildrenLines opts pos =
    lastAwaredFoldr
        ((<>) . (\(child, isLast) -> objTreeToLines opts (isLast : pos) child))
        DList.empty

lastAwaredFoldr :: ((a, Bool) -> b -> b) -> b -> [a] -> b
lastAwaredFoldr _ initial [] = initial
lastAwaredFoldr f initial [x] = f (x, True) (lastAwaredFoldr f initial [])
lastAwaredFoldr f initial (x : xs) = f (x, False) (lastAwaredFoldr f initial xs)

makePackageNameTextChunks :: Text -> [(Text, EntryLineColor)]
makePackageNameTextChunks pkgName =
    [(pkgName, EntryLineColor{color = White, intensity = Vivid})]

makeStoreObjectPathTextChunks :: Bool -> StoreObjectPath -> [(Text, EntryLineColor)]
makeStoreObjectPathTextChunks showFile objPath =
    [(text, EntryLineColor{color = Cyan, intensity = Dull})]
  where
    text =
        if showFile
            then StoreObjectPath.toText objPath
            else Text.take 8 (Nix32Hash.get (objPath ^. #hash))

makeDerivingPathTextChunks :: Bool -> DerivingPath -> [(Text, EntryLineColor)]
makeDerivingPathTextChunks showFile drvPath =
    [(text, EntryLineColor{color = Yellow, intensity = Dull})]
  where
    text =
        if showFile
            then DerivingPath.toText drvPath
            else "drv:" <> Text.take 8 (Nix32Hash.get (drvPath ^. #hash))
