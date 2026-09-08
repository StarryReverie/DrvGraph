module DrvGraph.Core.Model.DerivingPath
    ( DerivingPath (..)
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

-- | Path to a derivation file in the Nix store, without the store directory.
data DerivingPath = DerivingPath
    { dpHash :: Nix32Hash
    , dpName :: Text
    }
    deriving (Eq, Ord, Show)

type Parser = Parsec Void Text

-- | Parse a @DerivingPath@.
parse :: Parser DerivingPath
parse = do
    dpHash <- Nix32Hash.parse <?> "deriving path hash"
    MPC.char '-'
    dpName <-
        Text.pack <$> MP.manyTill MP.anySingle (MPC.string ".drv")
            <?> "deriving path name"
    pure DerivingPath{dpHash, dpName}

-- | Try to convert a @Text@ to @DerivingPath@.
fromText :: Text -> AppEither DerivingPath
fromText raw = withErrContext "could not parse deriving path" $ do
    case MP.runParser parse "" raw of
        Left err -> Left $ appError . Text.pack . MP.errorBundlePretty $ err
        Right dp -> Right dp

-- | Render a @DerivingPath@ datatype to a @Text@.
toText :: DerivingPath -> Text
toText dp = hash <> "-" <> name <> ".drv"
  where
    hash = Nix32Hash.get (dpHash dp)
    name = dpName dp

-- | Render a @DerivingPath@ datatype to a @FilePath@.
toFilePath :: DerivingPath -> FilePath
toFilePath = Text.unpack . toText

-- | Convert a @Text@ to a @DerivingPath@ or error.
uncheckedText :: Text -> DerivingPath
uncheckedText = unwrapRight . fromText
