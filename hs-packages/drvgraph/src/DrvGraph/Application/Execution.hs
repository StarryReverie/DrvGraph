module DrvGraph.Application.Execution
    ( AppEnvironment (..)
    , App (..)
    , runApp
    ) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT))

import DrvGraph.Application.Execution.CapDerivation qualified as CapDerivationImpl
import DrvGraph.Application.Execution.CapStoreObject qualified as CapStoreObjectImpl
import DrvGraph.Application.Execution.Environment (AppEnvironment (..))
import DrvGraph.Core.Capability.CapDerivation (CapDerivation (..))
import DrvGraph.Core.Capability.CapStoreObject (CapStoreObject (..), NarInfo)
import DrvGraph.Core.Error (AppExceptT)
import DrvGraph.Core.Model.Derivation (Derivation)
import DrvGraph.Core.Model.DerivingPath (DerivingPath)
import DrvGraph.Core.Model.StoreObjectPath (StoreObjectPath)

-- | Execution monad of the application.
newtype App a = App (ReaderT AppEnvironment IO a)
    deriving
        ( Applicative
        , Functor
        , Monad
        , MonadIO
        , MonadReader AppEnvironment
        , MonadThrow
        )

-- | Run the application with the given environment.
runApp :: App a -> AppEnvironment -> IO a
runApp (App app) = runReaderT app

instance CapDerivation App where
    loadDerivation :: FilePath -> DerivingPath -> AppExceptT App Derivation
    loadDerivation = CapDerivationImpl.loadDerivationImpl

instance CapStoreObject App where
    queryLocalStoreObject :: FilePath -> StoreObjectPath -> AppExceptT App Bool
    queryLocalStoreObject = CapStoreObjectImpl.queryLocalStoreObjectImpl

    queryRemoteStoreObject :: StoreObjectPath -> AppExceptT App (Maybe NarInfo)
    queryRemoteStoreObject = CapStoreObjectImpl.queryRemoteStoreObjectImpl
