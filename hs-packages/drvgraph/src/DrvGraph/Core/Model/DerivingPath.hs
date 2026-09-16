module DrvGraph.Core.Model.DerivingPath
    ( DerivingPath (..)
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

-- | Path to a derivation file in the Nix store, without the store directory.
data DerivingPath = DerivingPath
    { hash :: Nix32Hash
    , name :: Text
    }
    deriving (Eq, Ord, Show)

makeFieldLabelsNoPrefix ''DerivingPath

type Parser = Parsec Void Text

-- | Parse a @DerivingPath@.
parse :: Parser DerivingPath
parse = do
    hash <- Nix32Hash.parse <?> "deriving path hash"
    MegaparsecChar.char '-'
    name <-
        Text.pack <$> Megaparsec.manyTill Megaparsec.anySingle (MegaparsecChar.string ".drv")
            <?> "deriving path name"
    pure DerivingPath{hash, name}

-- | Try to convert a @Text@ to @DerivingPath@.
fromText :: Text -> AppEither DerivingPath
fromText raw = first (addAppErrorContext "could not parse deriving path") $ do
    body <- maybe (Left $ appError "missing .drv suffix") Right (Text.stripSuffix ".drv" raw)
    let (hashText, rest) = Text.splitAt 32 body
    hash <- Nix32Hash.fromText hashText
    name <- maybe (Left $ appError "missing '-' after hash") Right (Text.stripPrefix "-" rest)
    pure DerivingPath{hash, name}

-- | Render a @DerivingPath@ datatype to a @Text@.
toText :: DerivingPath -> Text
toText dp = hash <> "-" <> name <> ".drv"
  where
    hash = Nix32Hash.get (dp ^. #hash)
    name = dp ^. #name

-- | Render a @DerivingPath@ datatype to a @FilePath@.
toFilePath :: DerivingPath -> FilePath
toFilePath = Text.unpack . toText

-- | Convert a @Text@ to a @DerivingPath@ or error.
uncheckedText :: Text -> DerivingPath
uncheckedText = unwrapRight . fromText
