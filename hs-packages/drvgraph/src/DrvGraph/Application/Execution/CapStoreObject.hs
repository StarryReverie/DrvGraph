module DrvGraph.Application.Execution.CapStoreObject
    ( queryLocalStoreObjectImpl
    , queryRemoteStoreObjectImpl
    ) where

import Control.Exception.Safe (IOException, MonadCatch, MonadMask, bracket, throw)
import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, asks)
import Data.ByteString.Lazy qualified as LazyBytes
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Network.HTTP.Client (HttpException, Request (..))
import Network.HTTP.Client qualified as Http
import Network.HTTP.Types.Header qualified as HttpHeader
import Network.HTTP.Types.Status qualified as HttpStatus
import Network.URI (URI (..))
import Optics ((^.))
import System.Directory qualified as Directory
import System.FilePath ((</>))
import UnliftIO (MonadUnliftIO)
import UnliftIO.Async qualified as Async

import DrvGraph.Application.Execution.Environment (AppEnvironment)
import DrvGraph.Core.Error (AppEither, checkpointAppError, rethrowAsAppError, throwAppEither, throwAppErrorText, throwEitherAsAppError)
import DrvGraph.Core.Model.NarInfo (NarInfo (..))
import DrvGraph.Core.Model.Nix32Hash qualified as Nix32Hash
import DrvGraph.Core.Model.StoreObjectPath (StoreObjectPath)
import DrvGraph.Core.Model.StoreObjectPath qualified as StoreObjectPath

queryLocalStoreObjectImpl
    :: (MonadCatch m, MonadIO m)
    => FilePath -> StoreObjectPath -> m Bool
queryLocalStoreObjectImpl storeDir objPath = do
    let path = storeDir </> StoreObjectPath.toFilePath objPath

    checkpointAppError ("could not check existence of file " <> Text.pack path) $
        rethrowAsAppError @IOException (liftIO $ Directory.doesPathExist path)

queryRemoteStoreObjectImpl
    :: (MonadMask m, MonadReader AppEnvironment m, MonadUnliftIO m)
    => StoreObjectPath -> m (Maybe NarInfo)
queryRemoteStoreObjectImpl objPath = do
    servers <- asks (NonEmpty.toList . (^. #binaryCacheServers))

    requests <- do
        let file = (Text.unpack . Nix32Hash.get $ objPath ^. #hash) <> ".narinfo"
        forM (appendUrl file <$> servers) $ \url -> do
            initRequest <- liftIO $ Http.requestFromURI url
            let request = initRequest{requestHeaders = [(HttpHeader.hUserAgent, "DrvGraph/0.1.0.0")]}
            pure (url, request)

    raceRequests requests

appendUrl :: String -> URI -> URI
appendUrl file server@URI{uriPath} = server{uriPath = p}
  where
    p = case List.unsnoc uriPath of
        Just (_, c) | c == '/' -> uriPath <> file
        _ -> uriPath <> "/" <> file

raceRequests
    :: (MonadMask m, MonadReader AppEnvironment m, MonadUnliftIO m)
    => [(URI, Request)] -> m (Maybe NarInfo)
raceRequests requests =
    bracket
        (traverse (Async.async . uncurry sendRequest) requests)
        (traverse Async.cancel)
        (raceTasks Nothing . Set.fromList)
  where
    raceTasks lastException tasks
        | Set.null tasks = maybe (pure Nothing) throw lastException
        | otherwise = do
            (completed, res) <- Async.waitAnyCatch (Set.toList tasks)
            let updatedTasks = Set.delete completed tasks
            case res of
                Left ex -> raceTasks (Just ex) updatedTasks
                Right Nothing -> raceTasks lastException updatedTasks
                Right (Just val) -> pure $ Just val

sendRequest
    :: (MonadCatch m, MonadIO m, MonadReader AppEnvironment m)
    => URI -> Request -> m (Maybe NarInfo)
sendRequest uri request = do
    httpManager <- asks (^. #httpManager)

    response <- checkpointAppError ("could not send request to " <> uriText) $ do
        rethrowAsAppError @HttpException (liftIO $ Http.httpLbs request httpManager)

    content <- checkpointAppError ("could not read response of " <> uriText) $ do
        case Http.responseStatus response of
            HttpStatus.Status 200 _ -> do
                let bytes = LazyBytes.toStrict $ Http.responseBody response
                throwEitherAsAppError $ Just <$> TextEncoding.decodeUtf8' bytes
            HttpStatus.Status 403 _ -> pure Nothing
            HttpStatus.Status 404 _ -> pure Nothing
            status -> do
                throwAppErrorText $ "got response status code " <> Text.pack (show status)

    checkpointAppError ("could not parse narinfo from " <> uriText) $ do
        case content of
            Just raw -> throwAppEither $ Just <$> parseNarInfo raw
            Nothing -> pure Nothing
  where
    uriText = Text.pack (show uri)

parseNarInfo :: Text -> AppEither NarInfo
parseNarInfo raw = do
    let maybeReferences =
            List.take 1 . List.filter Maybe.isJust $
                Text.stripPrefix "References: " <$> Text.lines raw

    let referencesLine = case maybeReferences of
            [Just content] -> content
            _ -> ""

    let rawObjPaths = Text.words . Text.strip $ referencesLine
    references <- Set.fromList <$> traverse StoreObjectPath.fromText rawObjPaths

    pure NarInfo{references}
