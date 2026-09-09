module DrvGraph.Core.Capability.CapStoreObject
    ( CapStoreObject (..)
    , NarInfo (..)
    ) where

import Data.Set (Set)

import DrvGraph.Core.Error (AppExceptT)
import DrvGraph.Core.Model.StoreObjectPath (StoreObjectPath)

-- | Capability for querying store objects.
class (Monad m) => CapStoreObject m where
    -- | Check whether a store object exists in the local Nix store.
    queryLocalStoreObject
        :: FilePath
        -- ^ Nix store directory, e.g. @\/nix\/store@.
        -> StoreObjectPath
        -- ^ Path of the store object relative to the store directory.
        -> AppExceptT m Bool

    -- | Fetch the metadata of a store object from a remote store.
    queryRemoteStoreObject
        :: StoreObjectPath
        -- ^ Path of the store object relative to the store directory.
        -> AppExceptT m (Maybe NarInfo)

-- | Metadata of a NAR of a store object in the remote store.
newtype NarInfo = NarInfo
    { narInfoRefs :: Set StoreObjectPath
    }
