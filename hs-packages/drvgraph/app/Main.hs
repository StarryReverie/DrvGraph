module Main (main) where

import DrvGraph.Application (appMain)
import DrvGraph.Application.Argument (AppArguments (..), AppOptions (..))
import DrvGraph.Core.Model.DerivingPath qualified as DerivingPath

main :: IO ()
main = do
    let args =
            AppArguments
                { storeDir = "/nix/store/"
                , drvPath = DerivingPath.uncheckedText "csy6wvw1ypdrjyzx3rh7rdrhadvhbdh6-nixos-system-origin-26.11.20260911.eaad089.drv"
                , outName = "out"
                }
    let opts =
            AppOptions
                { showExisted = False
                , showVisited = False
                , showFile = False
                , substituters = Nothing
                }
    appMain args opts
