module DrvGraph.Core.TreeRepresentation
    ( StoreObjectTree (..)
    , DerivationTree (..)
    , TreeRepresentationOptions (..)
    , defaultOptions
    , depGraphToTreeRepresentation
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

data TreeRepresentationOptions = TreeRepresentationOptions
    { skipExisted :: Bool
    , skipVisited :: Bool
    }
    deriving (Eq, Show)

makeFieldLabelsNoPrefix ''StoreObjectTree
makeFieldLabelsNoPrefix ''DerivationTree
makeFieldLabelsNoPrefix ''TreeRepresentationOptions

data ToDisplayTreeState = ToDisplayTreeState
    { visitedObjPaths :: Set StoreObjectPath
    , visitedDrvPaths :: Set DerivingPath
    }

makeFieldLabelsNoPrefix ''ToDisplayTreeState

-- | Sensible default options for making a tree representation of the
-- derivation graph.
defaultOptions :: TreeRepresentationOptions
defaultOptions =
    TreeRepresentationOptions
        { skipExisted = True
        , skipVisited = True
        }

-- | Convert a traversal from a @StoreObjectPath@ in a @DepGraph@ to a tree
-- structure for displaying.
depGraphToTreeRepresentation
    :: TreeRepresentationOptions
    -> DepGraph
    -> StoreObjectPath
    -> Maybe StoreObjectTree
depGraphToTreeRepresentation opts graph path =
    evalState (recurseStoreObject opts True graph path) initial
  where
    initial =
        ToDisplayTreeState
            { visitedObjPaths = Set.empty
            , visitedDrvPaths = Set.empty
            }

recurseStoreObject
    :: TreeRepresentationOptions
    -> Bool
    -> DepGraph
    -> StoreObjectPath
    -> State ToDisplayTreeState (Maybe StoreObjectTree)
recurseStoreObject opts isTop graph objPath = do
    isVisited <- gets $ Set.member objPath . (^. #visitedObjPaths)
    if isVisited
        then
            if opts ^. #skipVisited
                then pure Nothing
                else pure $ Just StObjTreeVisited{objPath}
        else do
            modify $ #visitedObjPaths %~ Set.insert objPath

            case DepGraph.lookupObjNode objPath graph of
                Just ObjExisted
                    | not isTop && opts ^. #skipExisted -> pure Nothing
                    | otherwise -> pure $ Just StObjTreeExisted{objPath}
                Just ObjUnsynced{refPaths} -> recurseForUnsynced refPaths
                Just ObjUnbuilt{drvPath} -> recurseForUnbuilt drvPath
                Nothing -> pure Nothing
  where
    recurseForUnsynced refPaths = do
        refChildrenMaybes <- traverse (recurseStoreObject opts False graph) (Set.toList refPaths)
        let refChildren = Maybe.catMaybes refChildrenMaybes
        pure $ Just StObjTreeUnsynced{objPath, refChildren}

    recurseForUnbuilt :: DerivingPath -> State ToDisplayTreeState (Maybe StoreObjectTree)
    recurseForUnbuilt drvPath =
        case DepGraph.lookupDrvNodeAndIndegree drvPath graph of
            Just (drvNode, indegree)
                | indegree > 1 -> do
                    drvChild <- recurseDerivation opts graph drvPath
                    pure $ (\child -> StObjTreeUnbuilt{objPath, drvChild = child}) <$> drvChild
                | otherwise -> do
                    let refPaths = Set.toList (drvNode ^. #inputObjPaths)
                    objChildrenMaybes <- traverse (recurseStoreObject opts False graph) refPaths
                    let objChildren = Maybe.catMaybes objChildrenMaybes
                    pure $ Just StObjTreeUnbuiltWithDrv{objPath, drvPath, objChildren}
            Nothing -> pure Nothing

recurseDerivation
    :: TreeRepresentationOptions
    -> DepGraph
    -> DerivingPath
    -> (State ToDisplayTreeState) (Maybe DerivationTree)
recurseDerivation opts graph drvPath = do
    isVisited <- gets $ Set.member drvPath . (^. #visitedDrvPaths)
    if isVisited
        then
            if opts ^. #skipVisited
                then pure Nothing
                else pure $ Just DrvTreeVisited{drvPath}
        else do
            modify $ #visitedDrvPaths %~ Set.insert drvPath

            case DepGraph.lookupDrvNode drvPath graph of
                Just drvNode -> do
                    let refPaths = Set.toList (drvNode ^. #inputObjPaths)
                    maybeObjChildren <- traverse (recurseStoreObject opts False graph) refPaths
                    let objChildren = Maybe.catMaybes maybeObjChildren
                    pure $ Just DrvTreeUnbuilt{drvPath, objChildren}
                Nothing -> pure Nothing
