module DrvGraph.Application.Execution.CapStoreObject
    ( queryLocalStoreObjectImpl
    , queryRemoteStoreObjectImpl
    ) where

import Control.Exception (IOException, try)
import Control.Monad (forM)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader, asks)
import Data.ByteString.Lazy qualified as LazyBytes
import Data.List qualified as List
import Data.Maybe (isJust)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextE
import Network.HTTP.Client (HttpException, Request (..))
import Network.HTTP.Client qualified as Http
import Network.HTTP.Types.Header qualified as HttpHeader
import Network.HTTP.Types.Status qualified as HttpStatus
import Network.URI (URI (..))
import Optics ((^.))
import System.Directory qualified as Dir
import System.FilePath ((</>))

import DrvGraph.Application.Execution.Environment (AppEnvironment)
import DrvGraph.Core.Capability.CapStoreObject (NarInfo (..))
import DrvGraph.Core.Error (AppEither, AppExceptT, appError, exceptionToAppError, liftErr, withErrContext)
import DrvGraph.Core.Model.Nix32Hash qualified as Nix32Hash
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
    :: (MonadIO m, MonadReader AppEnvironment m, MonadThrow m)
    => StoreObjectPath -> AppExceptT m (Maybe NarInfo)
queryRemoteStoreObjectImpl objPath = do
    servers <- asks (^. #binaryCacheServers)

    requests <- do
        let file = (Text.unpack . Nix32Hash.get $ objPath ^. #hash) <> ".narinfo"
        forM (appendUrl file <$> servers) $ \url -> do
            initRequest <- Http.requestFromURI url
            let request = initRequest{requestHeaders = [(HttpHeader.hUserAgent, "DrvGraph/0.1.0.0")]}
            pure (url, request)

    sendRequestLoop requests

appendUrl :: String -> URI -> URI
appendUrl file server@URI{uriPath} = server{uriPath = p}
  where
    p = case List.unsnoc uriPath of
        Just (_, c) | c == '/' -> uriPath <> file
        _ -> uriPath <> "/" <> file

sendRequestLoop
    :: (MonadIO m, MonadReader AppEnvironment m)
    => [(URI, Request)] -> AppExceptT m (Maybe NarInfo)
sendRequestLoop requests = go requests Nothing
  where
    go [] Nothing = pure Nothing
    go [] (Just err) = throwError err
    go ((url, request) : rs) lastErr = do
        res <- runExceptT $ sendRequest url request
        case res of
            Left err -> go rs (Just err)
            Right Nothing -> go rs lastErr
            Right (Just narinfo) -> pure $ Just narinfo

sendRequest
    :: (MonadIO m, MonadReader AppEnvironment m)
    => URI -> Request -> AppExceptT m (Maybe NarInfo)
sendRequest uri request = do
    httpManager <- asks (^. #httpManager)

    response <- withErrContext ("could not send request to " <> uriText) $ do
        res <- liftIO $ try (Http.httpLbs request httpManager)

        case res of
            Left (ex :: HttpException) -> throwError $ exceptionToAppError ex
            Right bytes -> pure bytes

    content <- withErrContext ("could not read response of " <> uriText) $ do
        case Http.responseStatus response of
            HttpStatus.Status 200 _ -> do
                let bytes = LazyBytes.toStrict $ Http.responseBody response
                case TextE.decodeUtf8' bytes of
                    Left ex -> throwError $ exceptionToAppError ex
                    Right content -> pure $ Just content
            HttpStatus.Status 403 _ -> pure Nothing
            HttpStatus.Status 404 _ -> pure Nothing
            status -> do
                let errMsg = "got response status code " <> Text.pack (show status)
                throwError $ appError errMsg

    withErrContext ("could not parse narinfo from " <> uriText) $ do
        case content of
            Just raw -> liftErr $ Just <$> parseNarInfo raw
            Nothing -> pure Nothing
  where
    uriText = Text.pack (show uri)

parseNarInfo :: Text -> AppEither NarInfo
parseNarInfo raw = do
    let maybeReferences =
            List.take 1 . List.filter isJust $
                Text.stripPrefix "References: " <$> Text.lines raw

    let referencesLine = case maybeReferences of
            [Just content] -> content
            _ -> ""

    let rawObjPaths = Text.words . Text.strip $ referencesLine
    narInfoRefs <- Set.fromList <$> traverse StoreObjectPath.fromText rawObjPaths

    pure NarInfo{narInfoRefs}
