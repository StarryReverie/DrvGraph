module DrvGraph.Core.Export.Tree.Representation
    ( StoreObjectTree (..)
    , DerivationTree (..)
    , ToTreeOptions (..)
    , toTree
    ) where

import Control.Monad.State.Strict (State, evalState, gets, modify)
import Data.Maybe qualified as Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Optics ((%~), (^.))
import Optics.TH (makeFieldLabelsNoPrefix)

import DrvGraph.Core.Model.DepGraph (DepGraph, ObjNode (ObjExisted, ObjUnbuilt, ObjUnsynced))
import DrvGraph.Core.Model.DepGraph qualified as DepGraph
import DrvGraph.Core.Model.DerivingPath (DerivingPath)
import DrvGraph.Core.Model.StoreObjectPath (StoreObjectPath)

data StoreObjectTree
    = StObjTreeExisted
        { objPath :: StoreObjectPath
        }
    | StObjTreeUnsynced
        { objPath :: StoreObjectPath
        , refChildren :: [StoreObjectTree]
        , deriver :: Maybe DerivingPath
        }
    | StObjTreeUnbuilt
        { objPath :: StoreObjectPath
        , drvChild :: DerivationTree
        }
    | StObjTreeUnbuiltWithDrv
        { objPath :: StoreObjectPath
        , drvPath :: DerivingPath
        , objChildren :: [StoreObjectTree]
        }
    | StObjTreeVisited
        { objPath :: StoreObjectPath
        }
    deriving (Eq, Show)

data DerivationTree
    = DrvTreeUnbuilt
        { drvPath :: DerivingPath
        , objChildren :: [StoreObjectTree]
        }
    | DrvTreeVisited
        { drvPath :: DerivingPath
        }
    deriving (Eq, Show)

data ToTreeOptions = ToTreeOptions
    { includeExisted :: Bool
    , includeVisited :: Bool
    , maxDepth :: Maybe Int
    }
    deriving (Eq, Show)

makeFieldLabelsNoPrefix ''StoreObjectTree
makeFieldLabelsNoPrefix ''DerivationTree
makeFieldLabelsNoPrefix ''ToTreeOptions

data ToTreeState = ToTreeState
    { visitedObjPaths :: Set StoreObjectPath
    , visitedDrvPaths :: Set DerivingPath
    }

makeFieldLabelsNoPrefix ''ToTreeState

-- | Convert a traversal from a @StoreObjectPath@ in a @DepGraph@ to a tree
-- structure for displaying.
toTree
    :: ToTreeOptions
    -> DepGraph
    -> StoreObjectPath
    -> Maybe StoreObjectTree
toTree opts graph path =
    evalState (recurseStoreObject opts 0 graph path) initial
  where
    initial =
        ToTreeState
            { visitedObjPaths = Set.empty
            , visitedDrvPaths = Set.empty
            }

recurseStoreObject
    :: ToTreeOptions
    -> Int
    -> DepGraph
    -> StoreObjectPath
    -> State ToTreeState (Maybe StoreObjectTree)
recurseStoreObject opts depth graph objPath = do
    let isInRange = isInDepthRange (opts ^. #maxDepth) depth
    isVisited <- gets $ Set.member objPath . (^. #visitedObjPaths)

    case (isInRange, isVisited) of
        (False, _) -> pure Nothing
        (True, True)
            | opts ^. #includeVisited -> pure $ Just StObjTreeVisited{objPath}
            | otherwise -> pure Nothing
        (True, False) -> do
            modify $ #visitedObjPaths %~ Set.insert objPath

            case DepGraph.lookupObjNode objPath graph of
                Just ObjExisted
                    | depth == 0 || opts ^. #includeExisted -> pure $ Just StObjTreeExisted{objPath}
                    | otherwise -> pure Nothing
                Just ObjUnsynced{refPaths, deriver} -> recurseForUnsynced refPaths deriver
                Just ObjUnbuilt{drvPath} -> recurseForUnbuilt drvPath
                Nothing -> pure Nothing
  where
    recurseForUnsynced refPaths deriver = do
        refChildrenMaybes <- traverse (recurseStoreObject opts (depth + 1) graph) (Set.toList refPaths)
        let refChildren = Maybe.catMaybes refChildrenMaybes
        pure $ Just StObjTreeUnsynced{objPath, refChildren, deriver}

    recurseForUnbuilt :: DerivingPath -> State ToTreeState (Maybe StoreObjectTree)
    recurseForUnbuilt drvPath =
        case DepGraph.lookupDrvNodeAndIndegree drvPath graph of
            Just (drvNode, indegree)
                | indegree > 1 -> do
                    drvChild <- recurseDerivation opts (depth + 1) graph drvPath
                    pure $ (\child -> StObjTreeUnbuilt{objPath, drvChild = child}) <$> drvChild
                | otherwise -> do
                    let refPaths = Set.toList (drvNode ^. #inputObjPaths)
                    objChildrenMaybes <- traverse (recurseStoreObject opts (depth + 1) graph) refPaths
                    let objChildren = Maybe.catMaybes objChildrenMaybes
                    pure $ Just StObjTreeUnbuiltWithDrv{objPath, drvPath, objChildren}
            Nothing -> pure Nothing

recurseDerivation
    :: ToTreeOptions
    -> Int
    -> DepGraph
    -> DerivingPath
    -> (State ToTreeState) (Maybe DerivationTree)
recurseDerivation opts depth graph drvPath = do
    let isInRange = isInDepthRange (opts ^. #maxDepth) depth
    isVisited <- gets $ Set.member drvPath . (^. #visitedDrvPaths)

    case (isInRange, isVisited) of
        (False, _) -> pure Nothing
        (True, True)
            | opts ^. #includeVisited -> pure $ Just DrvTreeVisited{drvPath}
            | otherwise -> pure Nothing
        (True, False) -> do
            modify $ #visitedDrvPaths %~ Set.insert drvPath

            case DepGraph.lookupDrvNode drvPath graph of
                Just drvNode -> do
                    let refPaths = Set.toList (drvNode ^. #inputObjPaths)
                    maybeObjChildren <- traverse (recurseStoreObject opts (depth + 1) graph) refPaths
                    let objChildren = Maybe.catMaybes maybeObjChildren
                    pure $ Just DrvTreeUnbuilt{drvPath, objChildren}
                Nothing -> pure Nothing

isInDepthRange :: Maybe Int -> Int -> Bool
isInDepthRange maxDepth depth = maybe True (depth <=) maxDepth
