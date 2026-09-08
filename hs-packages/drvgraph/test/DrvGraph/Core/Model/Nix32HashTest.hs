module DrvGraph.Core.Model.Nix32HashTest
    ( genNix32Hash
    , genNix32HashText
    , hprop_fromValidText
    , hprop_fromTextOfInvalidLength
    , hprop_fromTextContainingInvalidChar
    ) where

import Data.Char qualified as Char
import Data.Either (isLeft, isRight)
import Data.Functor.Identity (Identity)
import Data.Text (Text)
import Data.Text qualified as Text
import Hedgehog (MonadGen (GenBase), Property, assert, forAll, property)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import DrvGraph.Core.Model.Nix32Hash (Nix32Hash)
import DrvGraph.Core.Model.Nix32Hash qualified as Nix32Hash

isEOUT :: Char -> Bool
isEOUT = (`elem` ['e', 'o', 'u', 't'])

genNix32Hash :: (GenBase m ~ Identity, MonadGen m) => m Nix32Hash
genNix32Hash = do
    text <- genNix32HashText
    case Nix32Hash.fromText text of
        Left err -> error $ "unreachable: " <> show err
        Right hash -> pure hash

genNix32HashText :: (GenBase m ~ Identity, MonadGen m) => m Text
genNix32HashText =
    Gen.text
        (Range.singleton 32)
        (Gen.filter (not . isEOUT) (Gen.choice [Gen.lower, Gen.digit]))

hprop_fromValidText :: Property
hprop_fromValidText = property $ do
    text <- forAll genNix32HashText
    assert $ isRight (Nix32Hash.fromText text)

hprop_fromTextOfInvalidLength :: Property
hprop_fromTextOfInvalidLength = property $ do
    text <-
        forAll $
            Gen.choice $
                flip fmap [Range.linear 0 31, Range.linear 33 50] $ \r ->
                    Gen.text r (Gen.filter (not . isEOUT) (Gen.choice [Gen.lower, Gen.digit]))
    assert $ isLeft (Nix32Hash.fromText text)

hprop_fromTextContainingInvalidChar :: Property
hprop_fromTextContainingInvalidChar = property $ do
    text <-
        forAll $
            Gen.filter
                (Text.any (\c -> isEOUT c || Char.isUpperCase c))
                (Gen.text (Range.singleton 32) Gen.alphaNum)
    assert $ isLeft (Nix32Hash.fromText text)
