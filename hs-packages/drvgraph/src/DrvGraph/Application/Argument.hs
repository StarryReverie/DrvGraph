module DrvGraph.Application.Argument
    ( AppArguments (..)
    , AppOptions (..)
    , AppOptionsWithDefault (..)
    ) where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Network.URI (URI)
import Optics.TH (makeFieldLabelsNoPrefix)

import DrvGraph.Core.Model.DerivingPath (DerivingPath)

data AppArguments = AppArguments
    { storeDir :: FilePath
    , drvPath :: DerivingPath
    , outName :: Text
    }

data AppOptions = AppOptions
    { showExisted :: Maybe Bool
    , showVisited :: Maybe Bool
    , showHash :: Maybe Bool
    , substituters :: Maybe (NonEmpty URI)
    }

data AppOptionsWithDefault = AppOptionsWithDefault
    { showExisted :: Bool
    , showVisited :: Bool
    , showHash :: Bool
    , substituters :: NonEmpty URI
    }

makeFieldLabelsNoPrefix ''AppArguments
makeFieldLabelsNoPrefix ''AppOptions
makeFieldLabelsNoPrefix ''AppOptionsWithDefault
