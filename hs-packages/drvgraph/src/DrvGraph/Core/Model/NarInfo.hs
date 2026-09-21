module DrvGraph.Core.Model.NarInfo
    ( NarInfo (..)
    , parse
    ) where

import Data.Function ((&))
import Data.Maybe qualified as Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Optics.TH (makeFieldLabelsNoPrefix)

import DrvGraph.Core.Error (AppEither)
import DrvGraph.Core.Model.DerivingPath (DerivingPath)
import DrvGraph.Core.Model.DerivingPath qualified as DerivingPath
import DrvGraph.Core.Model.StoreObjectPath (StoreObjectPath)
import DrvGraph.Core.Model.StoreObjectPath qualified as StoreObjectPath

-- | Metadata of a NAR of a store object in the remote store.
data NarInfo = NarInfo
    { references :: Set StoreObjectPath
    , deriver :: Maybe DerivingPath
    }
    deriving (Eq, Show)

makeFieldLabelsNoPrefix ''NarInfo

-- | Parse a @Text@ and extract essential @NarInfo@ fields.
parse :: Text -> AppEither NarInfo
parse raw = do
    references <- do
        let rawObjPaths = extractLine "References" raw
        Set.fromList <$> traverse StoreObjectPath.fromText rawObjPaths

    deriver <- do
        let rawDrvPath = Maybe.listToMaybe $ extractLine "Deriver" raw
        traverse DerivingPath.fromText rawDrvPath

    pure NarInfo{references, deriver}

extractLine :: Text -> Text -> [Text]
extractLine field raw =
    raw
        & Text.lines
        & Maybe.mapMaybe (Text.stripPrefix (field <> ": "))
        & Maybe.listToMaybe
        & Maybe.fromMaybe ""
        & Text.strip
        & Text.words
