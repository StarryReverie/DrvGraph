module DrvGraph.Application
    ( appMain
    ) where

import Control.Exception.Safe (MonadCatch)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text.IO qualified as TextIO

import DrvGraph.Application.Argument (AppArguments, AppOptions (..))
import DrvGraph.Application.Execution (runApp)
import DrvGraph.Core.Error (renderAppError, tryAppError)
import DrvGraph.Application.Initialization (appInit)

appMain :: (MonadCatch m, MonadIO m) => AppArguments -> AppOptions -> m ()
appMain args opts = do
    (env, optsDefault) <- appInit opts
    liftIO (runApp (tryAppError (app args optsDefault)) env) >>= \case
        Left err -> liftIO $ TextIO.putStrLn $ renderAppError err
        Right () -> pure ()
  where
    app args optsDefault = do
        pure ()
