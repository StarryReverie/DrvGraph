module DrvGraph.Core.Model.Nix32Hash
    ( Nix32Hash
    , pattern Nix32Hash
    , get
    , parse
    , fromText
    , uncheckedText
    ) where

import Control.Monad (replicateM)
import Data.Char qualified as Char
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as MP

import DrvGraph.Core.Error (AppEither, appError, unwrapRight, withErrContext)

-- | Nix's variant of the Base32 encoding, used for store path digest.
newtype Nix32Hash = Nix32HashInternal {hash :: Text}
    deriving (Eq, Ord, Show)

pattern Nix32Hash :: Text -> Nix32Hash
pattern Nix32Hash hash <- Nix32HashInternal hash
{-# COMPLETE Nix32Hash #-}

-- | Unwrap the inner hash value.
get :: Nix32Hash -> Text
get (Nix32HashInternal hash) = hash

type Parser = Parsec Void Text

-- | Parse a @Nix32Hash@.
parse :: Parser Nix32Hash
parse = do
    chars <- replicateM 32 (MP.satisfy charPred)
    pure $ Nix32HashInternal (Text.pack chars)
  where
    charPred c = Char.isDigit c || (Char.isAsciiLower c && c `notElem` ['e', 'o', 't', 'u'])

-- | Try to convert a @Text@ to @Nix32Hash@.
fromText :: Text -> AppEither Nix32Hash
fromText raw = withErrContext "could not parse Nix32 hash" $ do
    case MP.runParser (parse <* MP.eof) "" raw of
        Left err -> Left $ appError . Text.pack . MP.errorBundlePretty $ err
        Right hash -> Right hash

-- | Convert a @Text@ to @Nix32Hash@ or error
uncheckedText :: Text -> Nix32Hash
uncheckedText = unwrapRight . fromText
