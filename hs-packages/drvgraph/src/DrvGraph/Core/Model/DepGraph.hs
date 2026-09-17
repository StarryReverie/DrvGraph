module DrvGraph.Core.Model.DepGraph
    ( DepGraph (..)
    , ObjNode (..)
    , DrvNode (..)
    , empty
    , insertObjNode
    , insertDrvNode
    , lookupObjNode
    , lookupDrvNode
    , lookupDrvNodeAndIndegree
    ) where

import Data.Function ((&))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Set (Set)
import Optics ((%~), (^.))
import Optics.TH (makeFieldLabelsNoPrefix)

import DrvGraph.Core.Model.DerivingPath (DerivingPath (..))
import DrvGraph.Core.Model.StoreObjectPath (StoreObjectPath (..))

-- | Direct acyclic graph structure of store objects and derivations, whose
-- edges represent the dependencies between entities. Node @A@ points to node
-- @B@ iff. entity @A@ depends on entity @B@. The node's path is the map key.
data DepGraph = DepGraph
    { objNodes :: Map StoreObjectPath ObjNode
    , drvNodes :: Map DerivingPath DrvNode
    , drvNodeIndegrees :: Map DerivingPath Int
    }
    deriving (Eq, Show)

-- | State of a store object.
data ObjNode
    = ObjExisted
    | ObjUnsynced
        { refPaths :: Set StoreObjectPath
        }
    | ObjUnbuilt
        { drvPath :: DerivingPath
        }
    deriving (Eq, Show)

-- | Inputs of the derivation.
newtype DrvNode = DrvNode
    { inputObjPaths :: Set StoreObjectPath
    }
    deriving (Eq, Show)

makeFieldLabelsNoPrefix ''DepGraph
makeFieldLabelsNoPrefix ''ObjNode
makeFieldLabelsNoPrefix ''DrvNode

-- | Create an empty @DepGraph@.
empty :: DepGraph
empty =
    DepGraph
        { objNodes = Map.empty
        , drvNodes = Map.empty
        , drvNodeIndegrees = Map.empty
        }

-- | Insert a @ObjNode@ with the given @StoreObjectPath@.
insertObjNode :: StoreObjectPath -> ObjNode -> DepGraph -> DepGraph
insertObjNode path obj graph =
    case Map.lookup path (graph ^. #objNodes) of
        Just _ -> graph
        Nothing ->
            graph
                & #objNodes %~ Map.insert path obj
                & #drvNodeIndegrees %~ updateIndegree
  where
    updateIndegree = case obj of
        ObjExisted -> id
        ObjUnsynced{} -> id
        ObjUnbuilt{drvPath} -> Map.insertWith (+) drvPath 1

-- | Insert a @DrvNode@ with the given @DerivingPath@.
insertDrvNode :: DerivingPath -> DrvNode -> DepGraph -> DepGraph
insertDrvNode path drv graph = graph & #drvNodes %~ Map.insert path drv

-- | Lookup a @ObjNode@ with a @StoreObjectPath@.
lookupObjNode :: StoreObjectPath -> DepGraph -> Maybe ObjNode
lookupObjNode path = Map.lookup path . (^. #objNodes)

-- | Lookup a @DrvNode@ with a @DerivingPath@.
lookupDrvNode :: DerivingPath -> DepGraph -> Maybe DrvNode
lookupDrvNode path = Map.lookup path . (^. #drvNodes)

-- | Lookup a @DrvNode@ and its indegree with a @DerivingPath@.
lookupDrvNodeAndIndegree :: DerivingPath -> DepGraph -> Maybe (DrvNode, Int)
lookupDrvNodeAndIndegree path graph = do
    drvNode <- Map.lookup path (graph ^. #drvNodes)
    let indegree = Maybe.fromMaybe 0 (Map.lookup path (graph ^. #drvNodeIndegrees))
    pure (drvNode, indegree)
