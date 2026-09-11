module DrvGraph.Core.Model.StoreObjectPathTest
    ( genStoreObjectPath
    , genStoreObjectPathText
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

import DrvGraph.Core.Model.Nix32Hash qualified as Nix32Hash
import DrvGraph.Core.Model.Nix32HashTest qualified as Nix32HashTest
import DrvGraph.Core.Model.StoreObjectPath (StoreObjectPath (..))
import DrvGraph.Core.Model.StoreObjectPath qualified as StoreObjectPath

genStoreObjectPath :: (GenBase m ~ Identity, MonadGen m) => m StoreObjectPath
genStoreObjectPath = do
    hash <- Nix32HashTest.genNix32Hash
    name <- Gen.text (Range.linear 0 50) (Gen.choice [Gen.alphaNum, pure '-', pure '_'])
    pure $ StoreObjectPath{hash, name}

genStoreObjectPathText :: (GenBase m ~ Identity, MonadGen m) => m Text
genStoreObjectPathText = Text.pack . StoreObjectPath.toFilePath <$> genStoreObjectPath

unit_fromText :: IO ()
unit_fromText = do
    let Right actual = StoreObjectPath.fromText "wzr035k31pmpn2caabq8qwv1npg571z9-hello-2.12.3"
    let expected =
            StoreObjectPath
                { hash = Nix32Hash.uncheckedText "wzr035k31pmpn2caabq8qwv1npg571z9"
                , name = "hello-2.12.3"
                }
    actual @?= expected

hprop_toTextThenFromTextRoundTrip :: Property
hprop_toTextThenFromTextRoundTrip = property $ do
    sop <- forAll genStoreObjectPath
    let Right sop2 = StoreObjectPath.fromText . StoreObjectPath.toText $ sop
    sop === sop2

hprop_fromTextThenToTextRoundTrip :: Property
hprop_fromTextThenToTextRoundTrip = property $ do
    text <- forAll genStoreObjectPathText
    let text2 = StoreObjectPath.toText . StoreObjectPath.uncheckedText $ text
    text === text2
