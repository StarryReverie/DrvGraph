module DrvGraph.Core.Model.Nix32Hash
    ( Nix32Hash
    , pattern Nix32Hash
    , get
    , parse
    , fromText
    , uncheckedText
    ) where

import Data.Char qualified as Char
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as Megaparsec

import DrvGraph.Core.Error (AppEither, addAppErrorContext, appError, unwrapRight)

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

isNix32Char :: Char -> Bool
isNix32Char c = Char.isDigit c || (Char.isAsciiLower c && c `notElem` ['e', 'o', 't', 'u'])

-- | Parse a @Nix32Hash@.
parse :: Parser Nix32Hash
parse = do
    raw <- Megaparsec.takeP (Just "nix32 hash") 32
    if Text.all isNix32Char raw
        then pure $ Nix32HashInternal raw
        else fail "nix32 hash contains invalid character"

-- | Try to convert a @Text@ to @Nix32Hash@.
fromText :: Text -> AppEither Nix32Hash
fromText raw
    | Text.length raw == 32 && Text.all isNix32Char raw = Right $ Nix32HashInternal raw
    | otherwise = Left . addAppErrorContext "could not parse Nix32 hash" $ appError "invalid nix32 hash"

-- | Convert a @Text@ to @Nix32Hash@ or error
uncheckedText :: Text -> Nix32Hash
uncheckedText = unwrapRight . fromText
