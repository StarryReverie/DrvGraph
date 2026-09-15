module DrvGraph.Core.Capability.CapDerivation
    ( CapDerivation (..)
    ) where

import Control.Exception.Safe (MonadThrow)

import DrvGraph.Core.Model.Derivation (Derivation)
import DrvGraph.Core.Model.DerivingPath (DerivingPath)

-- | Capability for loading derivations from the Nix store.
class (MonadThrow m) => CapDerivation m where
    -- | Load and parse the derivation file from the store.
    loadDerivation
        :: FilePath
        -- ^ Nix store directory, e.g. @\/nix\/store@.
        -> DerivingPath
        -- ^ Path of the @.drv@ file relative to the store directory.
        -> m Derivation
