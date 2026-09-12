module DrvGraph.Application.Execution.Environment
    ( AppEnvironment (..)
    ) where

import Optics.TH (makeFieldLabelsNoPrefix)

-- | Environment (i.e. context) of the application
data AppEnvironment = AppEnvironment
    {
    }

makeFieldLabelsNoPrefix ''AppEnvironment
