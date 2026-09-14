module DrvGraph.Core.Capability.CapDerivation
    ( CapDerivation (..)
    ) where

import DrvGraph.Core.Error (AppExceptT)
import DrvGraph.Core.Model.Derivation (Derivation)
import DrvGraph.Core.Model.DerivingPath (DerivingPath)

-- | Capability for loading derivations from the Nix store.
class (Monad m) => CapDerivation m where
    -- | Load and parse the derivation file from the store.
    loadDerivation
        :: FilePath
        -- ^ Nix store directory, e.g. @\/nix\/store@.
        -> DerivingPath
        -- ^ Path of the @.drv@ file relative to the store directory.
        -> AppExceptT m Derivation
