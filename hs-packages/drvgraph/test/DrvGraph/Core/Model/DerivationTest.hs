module DrvGraph.Core.Model.DerivationTest
    ( unit_parseHello
    , unit_parseHelloSource
    ) where

import Data.FileEmbed qualified as Embed
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text.Encoding qualified as TextEncoding
import Test.Tasty.HUnit ((@?=))
import Text.Megaparsec as Megaparsec

import DrvGraph.Core.Model.Derivation (Derivation (..), DerivationOutput (..), OutputHash (..))
import DrvGraph.Core.Model.Derivation qualified as Derivation
import DrvGraph.Core.Model.DerivingPath qualified as DerivingPath
import DrvGraph.Core.Model.StoreObjectPath qualified as StoreObjectPath

unit_parseHello :: IO ()
unit_parseHello = do
    let input = TextEncoding.decodeUtf8 $(Embed.embedFileRelative "data/hello-2.12.3.drv")

    let Right actual = Megaparsec.runParser Derivation.parse "" input

    let expected =
            Derivation
                { inputDrvs =
                    Map.fromList
                        [
                            ( DerivingPath.uncheckedText "90yyxc3lkyxd23v52p9kh17p2j9r107c-stdenv-linux.drv"
                            , Set.fromList ["out"]
                            )
                        ,
                            ( DerivingPath.uncheckedText "a15s4f9z63cg8pg6w0gy9rf06jd3ra67-version-check-hook.drv"
                            , Set.fromList ["out"]
                            )
                        ,
                            ( DerivingPath.uncheckedText "fmyr3q5fikw8g50g1xf2vgs10hisxck9-hello-2.12.3.tar.gz.drv"
                            , Set.fromList ["out"]
                            )
                        ,
                            ( DerivingPath.uncheckedText "hplnhqsmnpr4gv35yf4cxvbalki3k308-bash-5.3p15.drv"
                            , Set.fromList ["out"]
                            )
                        ]
                , inputSrcs =
                    Set.fromList
                        [ StoreObjectPath.uncheckedText "l622p70vy8k5sh7y5wizi5f2mic6ynpg-source-stdenv.sh"
                        , StoreObjectPath.uncheckedText "shkw4qm9qcw5sc5n1k5jznc83ny02r39-default-builder.sh"
                        ]
                , outputs =
                    Map.fromList
                        [
                            ( "out"
                            , DerivationOutput
                                { path = StoreObjectPath.uncheckedText "wzr035k31pmpn2caabq8qwv1npg571z9-hello-2.12.3"
                                , hash = Nothing
                                }
                            )
                        ]
                }
    actual @?= expected

unit_parseHelloSource :: IO ()
unit_parseHelloSource = do
    let input = TextEncoding.decodeUtf8 $(Embed.embedFileRelative "data/hello-2.12.3.tar.gz.drv")

    let Right actual = Megaparsec.runParser Derivation.parse "" input

    let expected =
            Derivation
                { inputDrvs =
                    Map.fromList
                        [
                            ( DerivingPath.uncheckedText "hplnhqsmnpr4gv35yf4cxvbalki3k308-bash-5.3p15.drv"
                            , Set.fromList ["out"]
                            )
                        ,
                            ( DerivingPath.uncheckedText "iifp4fbhbyrc7zvy4lrxqzq6mv780mw7-curl-8.21.0.drv"
                            , Set.fromList ["dev"]
                            )
                        ,
                            ( DerivingPath.uncheckedText "kv2m32vy9pnbp4kgkzfbclsbyam14dvg-stdenv-linux-no-cc.drv"
                            , Set.fromList ["out"]
                            )
                        ,
                            ( DerivingPath.uncheckedText "yn8scfhr5k35i4wyn5rbf58qyhp55zlh-mirrors-list.drv"
                            , Set.fromList ["out"]
                            )
                        ]
                , inputSrcs =
                    Set.fromList
                        [ StoreObjectPath.uncheckedText "d0h3b1fj9hahy3lvh2kpny75w47la15s-builder.sh"
                        , StoreObjectPath.uncheckedText "l622p70vy8k5sh7y5wizi5f2mic6ynpg-source-stdenv.sh"
                        ]
                , outputs =
                    Map.fromList
                        [
                            ( "out"
                            , DerivationOutput
                                { path = StoreObjectPath.uncheckedText "wj7phsmi7ncidl8k00p489krqss7n9sd-hello-2.12.3.tar.gz"
                                , hash = Just OutputHash{algo = "sha256", val = "0d5f60154382fee10b114a1c34e785d8b1f492073ae2d3a6f7b147687b366aa0"}
                                }
                            )
                        ]
                }
    actual @?= expected
