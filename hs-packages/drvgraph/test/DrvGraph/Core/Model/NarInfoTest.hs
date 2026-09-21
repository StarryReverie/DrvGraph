module DrvGraph.Core.Model.NarInfoTest
    ( unit_parseNoReferences
    , unit_parseHasReferences
    ) where

import Data.FileEmbed qualified as Embed
import Data.Set qualified as Set
import Data.Text.Encoding qualified as TextEncoding
import Test.Tasty.HUnit ((@?=))

import DrvGraph.Core.Model.DerivingPath qualified as DerivingPath
import DrvGraph.Core.Model.NarInfo (NarInfo (..), parse)
import DrvGraph.Core.Model.StoreObjectPath qualified as StoreObjectPath

unit_parseNoReferences :: IO ()
unit_parseNoReferences = do
    let input = TextEncoding.decodeUtf8 $(Embed.embedFileRelative "data/linux-firmware-20260810-zstd.narinfo")

    let Right actual = parse input

    actual
        @?= NarInfo
            { references = Set.empty
            , deriver = Just $ DerivingPath.uncheckedText "9wjq923xmv9x7l3hdbklbfjyywv8ddgx-linux-firmware-20260810-zstd.drv"
            }

unit_parseHasReferences :: IO ()
unit_parseHasReferences = do
    let input = TextEncoding.decodeUtf8 $(Embed.embedFileRelative "data/jq-1.8.2-bin.narinfo")

    let Right actual = parse input

    actual
        @?= NarInfo
            { references =
                Set.fromList
                    [ StoreObjectPath.uncheckedText "3yhkyf209xh9pb7754vi8rfjh8mygwlq-oniguruma-6.9.10-lib"
                    , StoreObjectPath.uncheckedText "bkxqspw3p2d70my28z2skx5d84xkxi40-jq-1.8.2"
                    , StoreObjectPath.uncheckedText "hyz22a0l5b5kv7yfypw4ss6d36vbzg01-jq-1.8.2-bin"
                    , StoreObjectPath.uncheckedText "lm3pknxi0ipypy3lxh1wmm8wvvavdwrn-glibc-2.42-84"
                    ]
            , deriver = Just $ DerivingPath.uncheckedText "irkdk0czab57050824z86jn34d4iagar-jq-1.8.2.drv"
            }
