module DrvGraph.Application.Execution
    ( AppEnvironment (..)
    , App (..)
    , runApp
    ) where

import Control.Exception.Safe (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT))
import UnliftIO (MonadUnliftIO)

import DrvGraph.Application.Execution.CapDerivation qualified as CapDerivationImpl
import DrvGraph.Application.Execution.CapStoreObject qualified as CapStoreObjectImpl
import DrvGraph.Application.Execution.Environment (AppEnvironment (..))
import DrvGraph.Core.Capability.CapDerivation (CapDerivation (..))
import DrvGraph.Core.Capability.CapStoreObject (CapStoreObject (..), NarInfo)
import DrvGraph.Core.Model.Derivation (Derivation)
import DrvGraph.Core.Model.DerivingPath (DerivingPath)
import DrvGraph.Core.Model.StoreObjectPath (StoreObjectPath)

-- | Execution monad of the application.
newtype App a = App (ReaderT AppEnvironment IO a)
    deriving
        ( Applicative
        , Functor
        , Monad
        , MonadCatch
        , MonadIO
        , MonadMask
        , MonadReader AppEnvironment
        , MonadThrow
        , MonadUnliftIO
        )

-- | Run the application with the given environment.
runApp :: App a -> AppEnvironment -> IO a
runApp (App app) = runReaderT app

instance CapDerivation App where
    loadDerivation :: FilePath -> DerivingPath -> App Derivation
    loadDerivation = CapDerivationImpl.loadDerivationImpl

instance CapStoreObject App where
    queryLocalStoreObject :: FilePath -> StoreObjectPath -> App Bool
    queryLocalStoreObject = CapStoreObjectImpl.queryLocalStoreObjectImpl

    queryRemoteStoreObject :: StoreObjectPath -> App (Maybe NarInfo)
    queryRemoteStoreObject = CapStoreObjectImpl.queryRemoteStoreObjectImpl
