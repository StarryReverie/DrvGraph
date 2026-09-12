module DrvGraph.Application.Execution.CapStoreObject
    ( queryLocalStoreObjectImpl
    , queryRemoteStoreObjectImpl
    ) where

import Control.Exception (IOException, try)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text qualified as Text
import System.Directory qualified as Dir
import System.FilePath ((</>))

import DrvGraph.Core.Capability.CapStoreObject (NarInfo)
import DrvGraph.Core.Error (AppExceptT, exceptionToAppError, withErrContext)
import DrvGraph.Core.Model.StoreObjectPath (StoreObjectPath)
import DrvGraph.Core.Model.StoreObjectPath qualified as StoreObjectPath

queryLocalStoreObjectImpl
    :: (MonadIO m)
    => FilePath -> StoreObjectPath -> AppExceptT m Bool
queryLocalStoreObjectImpl storeDir objPath = do
    let path = storeDir </> StoreObjectPath.toFilePath objPath

    withErrContext ("could not check existence of file " <> Text.pack path) $ do
        res <- liftIO $ try (Dir.doesPathExist path)
        case res of
            Left (ex :: IOException) -> throwError $ exceptionToAppError ex
            Right exists -> pure exists

queryRemoteStoreObjectImpl
    :: (Monad m)
    => StoreObjectPath -> AppExceptT m (Maybe NarInfo)
queryRemoteStoreObjectImpl _ = do
    -- FIXME: Send actual HTTP requests to get narinfos.
    pure Nothing
