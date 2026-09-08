module DrvGraph.Core.Model.DerivingPathTest
    ( genDerivingPath
    , genDerivingPathText
    , unit_fromText
    , hprop_toTextThenFromTextRoundTrip
    , hprop_fromTextThenToTextRoundTrip
    ) where

import Data.Functor.Identity (Identity)
import Data.Text (Text)
import Data.Text qualified as Text
import Hedgehog (MonadGen (GenBase), Property, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty.HUnit ((@?=))

import DrvGraph.Core.Model.DerivingPath (DerivingPath (..))
import DrvGraph.Core.Model.DerivingPath qualified as DerivingPath
import DrvGraph.Core.Model.Nix32Hash qualified as Nix32Hash
import DrvGraph.Core.Model.Nix32HashTest qualified as Nix32HashTest

genDerivingPath :: (GenBase m ~ Identity, MonadGen m) => m DerivingPath
genDerivingPath = do
    dpHash <- Nix32HashTest.genNix32Hash
    dpName <- Gen.text (Range.linear 0 50) (Gen.choice [Gen.alphaNum, pure '-', pure '_'])
    pure $ DerivingPath{dpHash, dpName}

genDerivingPathText :: (GenBase m ~ Identity, MonadGen m) => m Text
genDerivingPathText = Text.pack . DerivingPath.toFilePath <$> genDerivingPath

unit_fromText :: IO ()
unit_fromText = do
    let Right actual = DerivingPath.fromText "g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-foo.drv"
    let expected =
            DerivingPath
                { dpHash = Nix32Hash.uncheckedText "g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q"
                , dpName = "foo"
                }
    actual @?= expected

hprop_toTextThenFromTextRoundTrip :: Property
hprop_toTextThenFromTextRoundTrip = property $ do
    dp <- forAll genDerivingPath
    let Right dp2 = DerivingPath.fromText . DerivingPath.toText $ dp
    dp === dp2

hprop_fromTextThenToTextRoundTrip :: Property
hprop_fromTextThenToTextRoundTrip = property $ do
    text <- forAll genDerivingPathText
    let text2 = DerivingPath.toText . DerivingPath.uncheckedText $ text
    text === text2
