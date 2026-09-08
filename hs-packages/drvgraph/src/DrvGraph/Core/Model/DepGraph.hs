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
-- @B@ iff. entity @A@ depends on entity @B@.
data DepGraph = DepGraph
    { dgObjNodes :: Map StoreObjectPath ObjNode
    , dgDrvNodes :: Map DerivingPath DrvNode
    }
    deriving (Eq, Show)

-- | Node which represents a store object and its state.
data ObjNode
    = ObjExisted
        { stObjPath :: StoreObjectPath
        }
    | ObjUnsynced
        { stObjPath :: StoreObjectPath
        , stDrvPath :: DerivingPath
        , stRefPaths :: Set StoreObjectPath
        }
    | ObjUnbuilt
        { stObjPath :: StoreObjectPath
        , stDrvPath :: DerivingPath
        }
    deriving (Eq, Show)

-- | Node which represents a derivation.
data DrvNode = DrvNode
    { drvPath :: DerivingPath
    , drvInputObjPaths :: Map StoreObjectPath (DerivingPath, Text)
    }
    deriving (Eq, Show)

-- | Create an empty @DepGraph@.
empty :: DepGraph
empty =
    DepGraph
        { dgObjNodes = Map.empty
        , dgDrvNodes = Map.empty
        }

-- | Insert a @ObjNode@.
insertObjNode :: ObjNode -> DepGraph -> DepGraph
insertObjNode obj graph = graph{dgObjNodes = Map.insert path obj (dgObjNodes graph)}
  where
    path = case obj of
        ObjExisted{stObjPath} -> stObjPath
        ObjUnsynced{stObjPath} -> stObjPath
        ObjUnbuilt{stObjPath} -> stObjPath

-- | Insert a @DrvNode@.
insertDrvNode :: DrvNode -> DepGraph -> DepGraph
insertDrvNode drv graph = graph{dgDrvNodes = Map.insert path drv (dgDrvNodes graph)}
  where
    path = drvPath drv

-- | Lookup a @ObjNode@ with a @StoreObjectPath@.
lookupObjNode :: StoreObjectPath -> DepGraph -> Maybe ObjNode
lookupObjNode path = Map.lookup path . dgObjNodes

-- | Lookup a @ObjNode@ with a @DerivingPath@.
lookupDrvNode :: DerivingPath -> DepGraph -> Maybe DrvNode
lookupDrvNode path = Map.lookup path . dgDrvNodes
