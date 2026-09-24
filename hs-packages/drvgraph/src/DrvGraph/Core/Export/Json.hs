module DrvGraph.Core.Export.Json
    ( exportJsonText
    ) where

import Data.Aeson.Text qualified as Aeson
import Data.Text (Text)
import Data.Text.Lazy qualified as LazyText

import DrvGraph.Core.Export.Json.Representation (toCombinedGraph)
import DrvGraph.Core.Model.DepGraph (DepGraph)
import DrvGraph.Core.Model.StoreObjectPath (StoreObjectPath)

exportJsonText :: (DepGraph, StoreObjectPath) -> Text
exportJsonText input =
    let graph = toCombinedGraph input
    in  LazyText.toStrict . Aeson.encodeToLazyText $ graph
