module DrvGraph.Core.Export.Tree
    ( ToTreeOptions (..)
    , FlattenTreeOptions (..)
    , exportTreeText
    ) where

import Control.Monad.Error.Class (MonadError (throwError))
import Data.Text (Text)
import Data.Text qualified as Text
import Optics ((^.))

import DrvGraph.Core.Error (AppEither, appError)
import DrvGraph.Core.Export.Tree.Flatten (FlattenTreeOptions (..), flattenTree)
import DrvGraph.Core.Export.Tree.Render (renderEntryLine)
import DrvGraph.Core.Export.Tree.Representation (ToTreeOptions (..), toTree)
import DrvGraph.Core.Model.DepGraph (DepGraph)
import DrvGraph.Core.Model.StoreObjectPath (StoreObjectPath)

exportTreeText :: ToTreeOptions -> FlattenTreeOptions -> (DepGraph, StoreObjectPath) -> AppEither Text
exportTreeText toTreeOpts flattenTreeOpts (depGraph, rootObjPath) = do
    tree <- case toTree toTreeOpts depGraph rootObjPath of
        Just tree -> pure tree
        Nothing -> throwError $ appError "no valid tree display"
    let flattened = flattenTree flattenTreeOpts tree
    let ls = Text.unlines $ renderEntryLine (flattenTreeOpts ^. #reversed) <$> flattened
    pure ls
