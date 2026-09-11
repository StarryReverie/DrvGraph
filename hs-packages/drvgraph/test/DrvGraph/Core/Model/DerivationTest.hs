module DrvGraph.Core.Model.DerivationTest
    ( unit_parseHello
    , unit_parseHelloSource
    ) where

import Data.FileEmbed qualified as Embed
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text.Encoding qualified as TE
import Test.Tasty.HUnit ((@?=))
import Text.Megaparsec as MP

import DrvGraph.Core.Model.Derivation (Derivation (..), DerivationOutput (..), OutputHash (..))
import DrvGraph.Core.Model.Derivation qualified as Derivation
import DrvGraph.Core.Model.DerivingPath qualified as DerivingPath
import DrvGraph.Core.Model.StoreObjectPath qualified as StoreObjectPath

unit_parseHello :: IO ()
unit_parseHello = do
    let input = TE.decodeUtf8 $(Embed.embedFileRelative "data/vlp8xby1jjmif5hdsck5vfdq4fljpvli-hello-2.12.3.drv")

    let Right actual = MP.runParser Derivation.parse "" input

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
                , platform = "x86_64-linux"
                , builder = "/nix/store/9ipfvwnqp1q8ijnmi5sxvlx9r8w34lw3-bash-5.3p15/bin/bash"
                , args =
                    [ "-e"
                    , "/nix/store/l622p70vy8k5sh7y5wizi5f2mic6ynpg-source-stdenv.sh"
                    , "/nix/store/shkw4qm9qcw5sc5n1k5jznc83ny02r39-default-builder.sh"
                    ]
                , envs =
                    Map.fromList
                        [ ("__json", "{\"NIX_MAIN_PROGRAM\":\"hello\",\"buildInputs\":[],\"builder\":\"/nix/store/9ipfvwnqp1q8ijnmi5sxvlx9r8w34lw3-bash-5.3p15/bin/bash\",\"cmakeFlags\":[],\"configureFlags\":[],\"depsBuildBuild\":[],\"depsBuildBuildPropagated\":[],\"depsBuildTarget\":[],\"depsBuildTargetPropagated\":[],\"depsHostHost\":[],\"depsHostHostPropagated\":[],\"depsTargetTarget\":[],\"depsTargetTargetPropagated\":[],\"doCheck\":true,\"doInstallCheck\":true,\"env\":{\"NIX_MAIN_PROGRAM\":\"hello\"},\"mesonFlags\":[],\"name\":\"hello-2.12.3\",\"nativeBuildInputs\":[\"/nix/store/811x2wvdhbkbkhcz3gdcfd3ai8sjinkv-version-check-hook\"],\"outputChecks\":{\"out\":{}},\"outputs\":[\"out\"],\"patches\":[],\"pname\":\"hello\",\"postInstallCheck\":\"stat \\\"${!outputBin}/bin/hello\\\"\\n\",\"propagatedBuildInputs\":[],\"propagatedNativeBuildInputs\":[],\"src\":\"/nix/store/wj7phsmi7ncidl8k00p489krqss7n9sd-hello-2.12.3.tar.gz\",\"stdenv\":\"/nix/store/dvj9bnk0vdbzzhn76wsnzk1dv6c6jbx1-stdenv-linux\",\"strictDeps\":false,\"system\":\"x86_64-linux\",\"version\":\"2.12.3\"}")
                        , ("out", "/nix/store/wzr035k31pmpn2caabq8qwv1npg571z9-hello-2.12.3")
                        ]
                }
    actual @?= expected

unit_parseHelloSource :: IO ()
unit_parseHelloSource = do
    let input = TE.decodeUtf8 $(Embed.embedFileRelative "data/fmyr3q5fikw8g50g1xf2vgs10hisxck9-hello-2.12.3.tar.gz.drv")

    let Right actual = MP.runParser Derivation.parse "" input

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
                , platform = "x86_64-linux"
                , builder = "/nix/store/9ipfvwnqp1q8ijnmi5sxvlx9r8w34lw3-bash-5.3p15/bin/bash"
                , args =
                    [ "-e"
                    , "/nix/store/l622p70vy8k5sh7y5wizi5f2mic6ynpg-source-stdenv.sh"
                    , "/nix/store/d0h3b1fj9hahy3lvh2kpny75w47la15s-builder.sh"
                    ]
                , envs =
                    Map.fromList
                        [ ("__json", "{\"SSL_CERT_FILE\":\"/no-cert-file.crt\",\"buildInputs\":[],\"builder\":\"/nix/store/9ipfvwnqp1q8ijnmi5sxvlx9r8w34lw3-bash-5.3p15/bin/bash\",\"cmakeFlags\":[],\"configureFlags\":[],\"curlOpts\":\"\",\"curlOptsList\":[],\"depsBuildBuild\":[],\"depsBuildBuildPropagated\":[],\"depsBuildTarget\":[],\"depsBuildTargetPropagated\":[],\"depsHostHost\":[],\"depsHostHostPropagated\":[],\"depsTargetTarget\":[],\"depsTargetTargetPropagated\":[],\"doCheck\":false,\"doInstallCheck\":false,\"downloadToTemp\":false,\"env\":{\"SSL_CERT_FILE\":\"/no-cert-file.crt\"},\"executable\":false,\"hash\":\"sha256-DV9gFUOC/uELEUocNOeF2LH0kgc64tOm97FHaHs2aqA=\",\"impureEnvVars\":[\"http_proxy\",\"https_proxy\",\"ftp_proxy\",\"all_proxy\",\"no_proxy\",\"HTTP_PROXY\",\"HTTPS_PROXY\",\"FTP_PROXY\",\"ALL_PROXY\",\"NO_PROXY\",\"NIX_SSL_CERT_FILE\",\"NIX_CURL_FLAGS\",\"NIX_HASHED_MIRRORS\",\"NIX_CONNECT_TIMEOUT\",\"NIX_MIRRORS_alsa\",\"NIX_MIRRORS_apache\",\"NIX_MIRRORS_bioc\",\"NIX_MIRRORS_bitlbee\",\"NIX_MIRRORS_centos\",\"NIX_MIRRORS_cpan\",\"NIX_MIRRORS_cran\",\"NIX_MIRRORS_debian\",\"NIX_MIRRORS_dub\",\"NIX_MIRRORS_fedora\",\"NIX_MIRRORS_gcc\",\"NIX_MIRRORS_gentoo\",\"NIX_MIRRORS_gnome\",\"NIX_MIRRORS_gnu\",\"NIX_MIRRORS_gnupg\",\"NIX_MIRRORS_hackage\",\"NIX_MIRRORS_hashedMirrors\",\"NIX_MIRRORS_ibiblioPubLinux\",\"NIX_MIRRORS_imagemagick\",\"NIX_MIRRORS_kde\",\"NIX_MIRRORS_kernel\",\"NIX_MIRRORS_luarocks\",\"NIX_MIRRORS_maven\",\"NIX_MIRRORS_mozilla\",\"NIX_MIRRORS_mysql\",\"NIX_MIRRORS_openbsd\",\"NIX_MIRRORS_opensuse\",\"NIX_MIRRORS_osdn\",\"NIX_MIRRORS_postgresql\",\"NIX_MIRRORS_pypi\",\"NIX_MIRRORS_qt\",\"NIX_MIRRORS_sageupstream\",\"NIX_MIRRORS_samba\",\"NIX_MIRRORS_savannah\",\"NIX_MIRRORS_sourceforge\",\"NIX_MIRRORS_steamrt\",\"NIX_MIRRORS_tcsh\",\"NIX_MIRRORS_testpypi\",\"NIX_MIRRORS_texhistoric\",\"NIX_MIRRORS_ubuntu\",\"NIX_MIRRORS_xfce\",\"NIX_MIRRORS_xorg\"],\"mesonFlags\":[],\"mirrorsFile\":\"/nix/store/5p6z3s8k8gflc7a6jrhy31ffvk4krs4q-mirrors-list\",\"name\":\"hello-2.12.3.tar.gz\",\"nativeBuildInputs\":[\"/nix/store/nz8xz8ax99skck3bpvaqv4807d6s257h-curl-8.21.0-dev\"],\"nixpkgsVersion\":\"26.11\",\"outputChecks\":{\"out\":{}},\"outputHash\":\"sha256-DV9gFUOC/uELEUocNOeF2LH0kgc64tOm97FHaHs2aqA=\",\"outputHashMode\":\"flat\",\"outputs\":[\"out\"],\"patches\":[],\"postFetch\":\"\",\"preferHashedMirrors\":false,\"preferLocalBuild\":true,\"propagatedBuildInputs\":[],\"propagatedNativeBuildInputs\":[],\"showURLs\":false,\"stdenv\":\"/nix/store/gk2waq8jq7b22kj6shg6qkya3yj06vdr-stdenv-linux-no-cc\",\"strictDeps\":false,\"system\":\"x86_64-linux\",\"urls\":[\"mirror://gnu/hello/hello-2.12.3.tar.gz\"]}")
                        , ("out", "/nix/store/wj7phsmi7ncidl8k00p489krqss7n9sd-hello-2.12.3.tar.gz")
                        ]
                }
    actual @?= expected
