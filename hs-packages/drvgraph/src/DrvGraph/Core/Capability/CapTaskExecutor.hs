module DrvGraph.Core.Capability.CapTaskExecutor
    ( CapTaskExecutor (..)
    ) where

import Control.Exception.Safe (MonadThrow)

-- | Capability for submitting async computations and waiting for their results.
class (MonadThrow m) => CapTaskExecutor m where
    -- | Inside a lexical scope, run an action that is provided with mechanisms
    -- of submitting computations and awaiting results.
    withTaskExecutor
        :: ((m a -> m (), m (Maybe a)) -> m r)
        -- ^ An action that is supplied with @submit@ and @await@ actions for
        -- task scheduling.
        -> m r
