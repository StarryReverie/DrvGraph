module DrvGraph.Core.Export.Json.Representation
    ( CombinedGraph (..)
    , CombinedNode (..)
    , toCombinedGraph
    ) where

import Data.Aeson (ToJSON (toJSON), Value, (.=))
import Data.Aeson qualified as Aeson
import Data.Function ((&))
import Data.Map qualified as Map
import Data.Map.Strict (Map)
import Data.Maybe qualified as Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Optics ((^.))
import Optics.TH (makeFieldLabelsNoPrefix)

import DrvGraph.Core.Model.DepGraph (DepGraph, DrvNode (..), ObjNode (..))
import DrvGraph.Core.Model.DepGraph qualified as DepGraph
import DrvGraph.Core.Model.DerivingPath (DerivingPath)
import DrvGraph.Core.Model.DerivingPath qualified as DerivingPath
import DrvGraph.Core.Model.StoreObjectPath (StoreObjectPath)
import DrvGraph.Core.Model.StoreObjectPath qualified as StoreObjectPath

data CombinedGraph = CombinedGraph
    { root :: StoreObjectPath
    , nodes :: Map Text CombinedNode
    , edges :: Map Text (Set Text)
    }
    deriving (Eq, Show)

data CombinedNode
    = CbNodeObjExisted
        { objPath :: StoreObjectPath
        }
    | CbNodeObjUnsynced
        { objPath :: StoreObjectPath
        , deriver :: Maybe DerivingPath
        }
    | CbNodeObjUnbuilt
        { objPath :: StoreObjectPath
        }
    | CbNodeObjUnbuiltWithDrv
        { objPath :: StoreObjectPath
        , drvPath :: DerivingPath
        }
    | CbNodeDrvUnbuilt
        { drvPath :: DerivingPath
        }
    deriving (Eq, Show)

makeFieldLabelsNoPrefix ''CombinedGraph
makeFieldLabelsNoPrefix ''CombinedNode

-- | Convert the internal @DepGraph@ to a representation that can be encoded to
-- a JSON.
toCombinedGraph :: (DepGraph, StoreObjectPath) -> CombinedGraph
toCombinedGraph (depGraph, rootObjPath) =
    let (objNodes, objEdges) =
            Map.toList (depGraph ^. #objNodes)
                & fmap (uncurry (objNodeToCombinded depGraph))
                & unzip
        (drvNodes, drvEdges) =
            Map.toList (depGraph ^. #drvNodes)
                & Maybe.mapMaybe (uncurry (drvNodeToCombined depGraph))
                & unzip
    in  CombinedGraph
            { root = rootObjPath
            , nodes = Map.fromList (objNodes <> drvNodes)
            , edges = Map.fromList (objEdges <> drvEdges)
            }

objNodeToCombinded
    :: DepGraph
    -> StoreObjectPath
    -> ObjNode
    -> ((Text, CombinedNode), (Text, Set Text))
objNodeToCombinded depGraph objPath objNode = case objNode of
    ObjExisted ->
        ( (nodeId, CbNodeObjExisted{objPath})
        , (nodeId, Set.empty)
        )
    ObjUnsynced{refPaths, deriver} ->
        ( (nodeId, CbNodeObjUnsynced{objPath, deriver})
        , (nodeId, Set.map StoreObjectPath.toText refPaths)
        )
    ObjUnbuilt{drvPath} -> case DepGraph.lookupDrvNodeAndIndegree drvPath depGraph of
        Just (DrvNode{inputObjPaths}, indegree)
            | indegree == 1 ->
                ( (nodeId, CbNodeObjUnbuiltWithDrv{objPath, drvPath})
                , (nodeId, Set.map StoreObjectPath.toText inputObjPaths)
                )
        _ ->
            ( (nodeId, CbNodeObjUnbuilt{objPath})
            , (nodeId, Set.singleton (DerivingPath.toText drvPath))
            )
  where
    nodeId = StoreObjectPath.toText objPath

drvNodeToCombined
    :: DepGraph
    -> DerivingPath
    -> DrvNode
    -> Maybe ((Text, CombinedNode), (Text, Set Text))
drvNodeToCombined depGraph drvPath DrvNode{inputObjPaths} =
    if maybe True ((== 1) . snd) (DepGraph.lookupDrvNodeAndIndegree drvPath depGraph)
        then Nothing
        else
            Just
                ( (nodeId, CbNodeDrvUnbuilt{drvPath})
                , (nodeId, Set.map StoreObjectPath.toText inputObjPaths)
                )
  where
    nodeId = DerivingPath.toText drvPath

instance ToJSON CombinedGraph where
    toJSON :: CombinedGraph -> Value
    toJSON CombinedGraph{root, nodes, edges} =
        Aeson.object
            [ "root" .= StoreObjectPath.toText root
            , "nodes" .= nodes
            , "edges" .= edges
            ]

instance ToJSON CombinedNode where
    toJSON :: CombinedNode -> Value
    toJSON CbNodeObjExisted{objPath} =
        Aeson.object
            [ "type" .= ("ObjExisted" :: Text)
            , "objPath" .= StoreObjectPath.toText objPath
            ]
    toJSON CbNodeObjUnsynced{objPath, deriver} =
        Aeson.object
            [ "type" .= ("ObjUnsynced" :: Text)
            , "objPath" .= StoreObjectPath.toText objPath
            , "deriver" .= fmap DerivingPath.toText deriver
            ]
    toJSON CbNodeObjUnbuilt{objPath} =
        Aeson.object
            [ "type" .= ("ObjUnbuilt" :: Text)
            , "objPath" .= StoreObjectPath.toText objPath
            ]
    toJSON CbNodeObjUnbuiltWithDrv{objPath, drvPath} =
        Aeson.object
            [ "type" .= ("ObjUnbuiltWithDrv" :: Text)
            , "objPath" .= StoreObjectPath.toText objPath
            , "drvPath" .= DerivingPath.toText drvPath
            ]
    toJSON CbNodeDrvUnbuilt{drvPath} =
        Aeson.object
            [ "type" .= ("DrvUnbuilt" :: Text)
            , "drvPath" .= DerivingPath.toText drvPath
            ]
