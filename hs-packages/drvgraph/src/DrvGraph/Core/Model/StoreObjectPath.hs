module DrvGraph.Core.Model.StoreObjectPath
    ( StoreObjectPath (..)
    , parse
    , fromText
    , toText
    , toFilePath
    , uncheckedText
    ) where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Text.Megaparsec (Parsec, (<?>))
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC

import DrvGraph.Core.Error (AppEither, appError, unwrapRight, withErrContext)
import DrvGraph.Core.Model.Nix32Hash (Nix32Hash)
import DrvGraph.Core.Model.Nix32Hash qualified as Nix32Hash

-- | Path to a store object in the Nix store, without the store directory.
data StoreObjectPath = StoreObjectPath
    { stObjHash :: Nix32Hash
    , stObjName :: Text
    }
    deriving (Eq, Ord, Show)

type Parser = Parsec Void Text

-- | Parse a @StoreObjectPath@.
parse :: Parser StoreObjectPath
parse = do
    stObjHash <- Nix32Hash.parse <?> "store object path hash"
    MPC.char '-'
    stObjName <- MP.takeWhileP Nothing (/= '/') <?> "store object path name"
    pure StoreObjectPath{stObjHash, stObjName}

-- | Try to convert a @Text@ to a @StoreObjectPath@.
fromText :: Text -> AppEither StoreObjectPath
fromText raw = withErrContext "could not parse store object path" $ do
    case MP.runParser parse "" raw of
        Left err -> Left $ appError . Text.pack . MP.errorBundlePretty $ err
        Right sop -> Right sop

-- | Render a @StoreObjectPath@ datatype to a @Text@.
toText :: StoreObjectPath -> Text
toText sop = hash <> "-" <> name
  where
    hash = Nix32Hash.get (stObjHash sop)
    name = stObjName sop

-- | Render a @StoreObjectPath@ datatype to a @FilePath@.
toFilePath :: StoreObjectPath -> FilePath
toFilePath = Text.unpack . toText

-- | Convert a @Text@ to a @StoreObjectPath@ or error.
uncheckedText :: Text -> StoreObjectPath
uncheckedText = unwrapRight . fromText
