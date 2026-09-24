module DrvGraph.Application.Argument
    ( AppArguments (..)
    , AppOptions (..)
    , AppOptionsWithDefault (..)
    , ExportFormat (..)
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
    { showExisted :: Bool
    , showVisited :: Bool
    , showFile :: Bool
    , reversed :: Bool
    , maxDepth :: Maybe Int
    , substituters :: Maybe (NonEmpty URI)
    , format :: Maybe ExportFormat
    }

data AppOptionsWithDefault = AppOptionsWithDefault
    { showExisted :: Bool
    , showVisited :: Bool
    , showFile :: Bool
    , reversed :: Bool
    , maxDepth :: Maybe Int
    , substituters :: NonEmpty URI
    , format :: ExportFormat
    }

data ExportFormat
    = ExportTree
    | ExportJson
    deriving (Eq, Show)

makeFieldLabelsNoPrefix ''AppArguments
makeFieldLabelsNoPrefix ''AppOptions
makeFieldLabelsNoPrefix ''AppOptionsWithDefault
