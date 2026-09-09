module DrvGraph.Core.Model.DepGraph
    ( DepGraph (..)
    , ObjNode (..)
    , DrvNode (..)
    , empty
    , insertObjNode
    , insertDrvNode
    , lookupObjNode
    , lookupDrvNode
    ) where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Text (Text)

import DrvGraph.Core.Model.DerivingPath (DerivingPath (..))
import DrvGraph.Core.Model.StoreObjectPath (StoreObjectPath (..))

-- | Direct acyclic graph structure of store objects and derivations, whose
-- edges represent the dependencies between entities. Node @A@ points to node
-- @B@ iff. entity @A@ depends on entity @B@. The node's path is the map key.
data DepGraph = DepGraph
    { dgObjNodes :: Map StoreObjectPath ObjNode
    , dgDrvNodes :: Map DerivingPath DrvNode
    }
    deriving (Eq, Show)

-- | State of a store object at @dgObjNodes@'s key.
data ObjNode
    = ObjExisted
    | ObjUnsynced
        { stDrvPath :: DerivingPath
        , stRefPaths :: Set StoreObjectPath
        }
    | ObjUnbuilt
        { stDrvPath :: DerivingPath
        }
    deriving (Eq, Show)

-- | Inputs of the derivation at @dgDrvNodes@'s key.
newtype DrvNode = DrvNode
    { drvInputObjPaths :: Map StoreObjectPath (DerivingPath, Text)
    }
    deriving (Eq, Show)

-- | Create an empty @DepGraph@.
empty :: DepGraph
empty =
    DepGraph
        { dgObjNodes = Map.empty
        , dgDrvNodes = Map.empty
        }

-- | Insert a @ObjNode@ with the given @StoreObjectPath@.
insertObjNode :: StoreObjectPath -> ObjNode -> DepGraph -> DepGraph
insertObjNode path obj graph = graph{dgObjNodes = Map.insert path obj (dgObjNodes graph)}

-- | Insert a @DrvNode@ with the given @DerivingPath@.
insertDrvNode :: DerivingPath -> DrvNode -> DepGraph -> DepGraph
insertDrvNode path drv graph = graph{dgDrvNodes = Map.insert path drv (dgDrvNodes graph)}

-- | Lookup a @ObjNode@ with a @StoreObjectPath@.
lookupObjNode :: StoreObjectPath -> DepGraph -> Maybe ObjNode
lookupObjNode path = Map.lookup path . dgObjNodes

-- | Lookup a @DrvNode@ with a @DerivingPath@.
lookupDrvNode :: DerivingPath -> DepGraph -> Maybe DrvNode
lookupDrvNode path = Map.lookup path . dgDrvNodes
