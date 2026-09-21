module DrvGraph.Core.Model.NarInfo
    ( NarInfo (..)
    , parse
    ) where

import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Optics.TH (makeFieldLabelsNoPrefix)

import DrvGraph.Core.Error (AppEither)
import DrvGraph.Core.Model.StoreObjectPath (StoreObjectPath)
import DrvGraph.Core.Model.StoreObjectPath qualified as StoreObjectPath

-- | Metadata of a NAR of a store object in the remote store.
newtype NarInfo = NarInfo
    { references :: Set StoreObjectPath
    }

makeFieldLabelsNoPrefix ''NarInfo

parse :: Text -> AppEither NarInfo
parse raw = do
    let maybeReferences =
            List.take 1 . List.filter Maybe.isJust $
                Text.stripPrefix "References: " <$> Text.lines raw

    let referencesLine = case maybeReferences of
            [Just content] -> content
            _ -> ""

    let rawObjPaths = Text.words . Text.strip $ referencesLine
    references <- Set.fromList <$> traverse StoreObjectPath.fromText rawObjPaths

    pure NarInfo{references}
