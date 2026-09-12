module DrvGraph.Core.TreeRepresentation
    ( StoreObjectTree (..)
    , DerivationTree (..)
    , depGraphToTreeRepresentation
    ) where

import Control.Applicative (empty)
import Control.Monad.State.Strict (State, evalState, gets, modify)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Map qualified as Map
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

makeFieldLabelsNoPrefix ''StoreObjectTree
makeFieldLabelsNoPrefix ''DerivationTree

data ToDisplayTreeState = ToDisplayTreeState
    { visitedObjPaths :: Set StoreObjectPath
    , visitedDrvPaths :: Set DerivingPath
    }

makeFieldLabelsNoPrefix ''ToDisplayTreeState

-- | Convert a traversal from a @StoreObjectPath@ in a @DepGraph@ to a tree
-- structure for displaying.
depGraphToTreeRepresentation :: DepGraph -> StoreObjectPath -> Maybe StoreObjectTree
depGraphToTreeRepresentation graph path = evalState (runMaybeT (recurseStoreObject graph path)) initial
  where
    initial =
        ToDisplayTreeState
            { visitedObjPaths = Set.empty
            , visitedDrvPaths = Set.empty
            }

recurseStoreObject :: DepGraph -> StoreObjectPath -> MaybeT (State ToDisplayTreeState) StoreObjectTree
recurseStoreObject graph objPath = do
    isVisited <- gets $ Set.member objPath . (^. #visitedObjPaths)
    if isVisited
        then pure StObjTreeVisited{objPath}
        else do
            modify $ #visitedObjPaths %~ Set.insert objPath

            objNode <- ofMaybe $ DepGraph.lookupObjNode objPath graph
            case objNode of
                ObjExisted -> pure StObjTreeExisted{objPath}
                ObjUnsynced{refPaths} -> recurseForUnsynced refPaths
                ObjUnbuilt{drvPath} -> recurseForUnbuilt drvPath
  where
    recurseForUnsynced refPaths = do
        refChildren <- traverse (recurseStoreObject graph) (Set.toList refPaths)
        pure StObjTreeUnsynced{objPath, refChildren}

    recurseForUnbuilt drvPath = do
        (drvNode, indegree) <- ofMaybe $ DepGraph.lookupDrvNodeAndIndegree drvPath graph
        if indegree > 1
            then do
                drvChild <- recurseDerivation graph drvPath
                pure StObjTreeUnbuilt{objPath, drvChild}
            else do
                let refPaths = Map.keys (drvNode ^. #inputObjPaths)
                objChildren <- traverse (recurseStoreObject graph) refPaths
                pure StObjTreeUnbuiltWithDrv{objPath, drvPath, objChildren}

recurseDerivation :: DepGraph -> DerivingPath -> MaybeT (State ToDisplayTreeState) DerivationTree
recurseDerivation graph drvPath = do
    isVisited <- gets $ Set.member drvPath . (^. #visitedDrvPaths)
    if isVisited
        then pure DrvTreeVisited{drvPath}
        else do
            modify $ #visitedDrvPaths %~ Set.insert drvPath

            drvNode <- ofMaybe $ DepGraph.lookupDrvNode drvPath graph
            let refPaths = Map.keys (drvNode ^. #inputObjPaths)
            objChildren <- traverse (recurseStoreObject graph) refPaths
            pure DrvTreeUnbuilt{drvPath, objChildren}

ofMaybe :: (Monad m) => Maybe a -> MaybeT m a
ofMaybe = maybe empty pure
