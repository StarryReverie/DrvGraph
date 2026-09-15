module DrvGraph.Application.Execution.Environment
    ( AppEnvironment (..)
    ) where

import Data.List.NonEmpty (NonEmpty)
import Network.HTTP.Client (Manager)
import Network.URI (URI)
import Optics.TH (makeFieldLabelsNoPrefix)

-- | Environment (i.e. context) of the application
data AppEnvironment = AppEnvironment
    { httpManager :: Manager
    , binaryCacheServers :: NonEmpty URI
    }

makeFieldLabelsNoPrefix ''AppEnvironment
