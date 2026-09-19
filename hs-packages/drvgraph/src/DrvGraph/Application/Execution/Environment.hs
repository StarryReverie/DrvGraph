module DrvGraph.Application.Execution.Environment
    ( AppEnvironment (..)
    ) where

import Data.List.NonEmpty (NonEmpty)
import Network.HTTP.Client (Manager)
import Network.URI (URI)
import Optics.TH (makeFieldLabelsNoPrefix)
import StmContainers.Map qualified as StmMap

import DrvGraph.Core.Model.Derivation (Derivation)

-- | Environment (i.e. context) of the application
data AppEnvironment = AppEnvironment
    { numMaxJobs :: Int
    , httpManager :: Manager
    , derivationCache :: StmMap.Map FilePath Derivation
    , binaryCacheServers :: NonEmpty URI
    }

makeFieldLabelsNoPrefix ''AppEnvironment
