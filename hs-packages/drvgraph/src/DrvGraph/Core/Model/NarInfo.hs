module DrvGraph.Core.Model.NarInfo
    ( NarInfo (..)
    ) where

import Data.Set (Set)

import DrvGraph.Core.Model.StoreObjectPath (StoreObjectPath)

-- | Metadata of a NAR of a store object in the remote store.
newtype NarInfo = NarInfo
    { references :: Set StoreObjectPath
    }
