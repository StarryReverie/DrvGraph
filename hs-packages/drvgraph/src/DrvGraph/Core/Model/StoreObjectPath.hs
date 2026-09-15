module DrvGraph.Core.Model.StoreObjectPath
    ( StoreObjectPath (..)
    , parse
    , fromText
    , toText
    , toFilePath
    , uncheckedText
    ) where

import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Optics ((^.))
import Optics.TH (makeFieldLabelsNoPrefix)
import Text.Megaparsec (Parsec, (<?>))
import Text.Megaparsec qualified as Megaparsec
import Text.Megaparsec.Char qualified as MegaparsecChar

import DrvGraph.Core.Error (AppEither, addAppErrorContext, appError, unwrapRight)
import DrvGraph.Core.Model.Nix32Hash (Nix32Hash)
import DrvGraph.Core.Model.Nix32Hash qualified as Nix32Hash

-- | Path to a store object in the Nix store, without the store directory.
data StoreObjectPath = StoreObjectPath
    { hash :: Nix32Hash
    , name :: Text
    }
    deriving (Eq, Ord, Show)

makeFieldLabelsNoPrefix ''StoreObjectPath

type Parser = Parsec Void Text

-- | Parse a @StoreObjectPath@.
parse :: Parser StoreObjectPath
parse = do
    hash <- Nix32Hash.parse <?> "store object path hash"
    MegaparsecChar.char '-'
    name <- Megaparsec.takeWhileP Nothing (/= '/') <?> "store object path name"
    pure StoreObjectPath{hash, name}

-- | Try to convert a @Text@ to a @StoreObjectPath@.
fromText :: Text -> AppEither StoreObjectPath
fromText raw = first (addAppErrorContext "could not parse store object path") $ do
    case Megaparsec.runParser parse "" raw of
        Left err -> Left $ appError . Text.pack . Megaparsec.errorBundlePretty $ err
        Right sop -> Right sop

-- | Render a @StoreObjectPath@ datatype to a @Text@.
toText :: StoreObjectPath -> Text
toText sop = hash <> "-" <> name
  where
    hash = Nix32Hash.get (sop ^. #hash)
    name = sop ^. #name

-- | Render a @StoreObjectPath@ datatype to a @FilePath@.
toFilePath :: StoreObjectPath -> FilePath
toFilePath = Text.unpack . toText

-- | Convert a @Text@ to a @StoreObjectPath@ or error.
uncheckedText :: Text -> StoreObjectPath
uncheckedText = unwrapRight . fromText
