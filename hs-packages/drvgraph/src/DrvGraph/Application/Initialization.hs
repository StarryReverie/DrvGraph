module DrvGraph.Application.Initialization
    ( appInit
    ) where

import Control.Concurrent.STM (atomically)
import Control.Exception.Safe (IOException, MonadCatch)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString qualified as Bytes
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe qualified as Maybe
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Network.HTTP.Client (ManagerSettings (..))
import Network.HTTP.Client qualified as Http
import Network.HTTP.Client.TLS qualified as HttpTls
import Network.URI (URI)
import Network.URI qualified as Uri
import Optics ((^.))
import StmContainers.Map qualified as StmMap
import UnliftIO.Concurrent qualified as Concurrent
import UnliftIO.Directory qualified as Directory

import DrvGraph.Application.Argument (AppOptions (..), AppOptionsWithDefault (..), ExportFormat (ExportTree))
import DrvGraph.Application.Execution.Environment (AppEnvironment (..))
import DrvGraph.Core.Error (checkpointAppError, rethrowAsAppError, throwAppErrorText, throwEitherAsAppError)

appInit :: (MonadCatch m, MonadIO m) => AppOptions -> m (AppEnvironment, AppOptionsWithDefault)
appInit opts = do
    numMaxJobs <- Concurrent.getNumCapabilities

    substituters <- maybe readSubstitutersFromNixConf pure (opts ^. #substituters)

    let optsDefault =
            AppOptionsWithDefault
                { showExisted = opts ^. #showExisted
                , showVisited = opts ^. #showVisited
                , showFile = opts ^. #showFile
                , reversed = opts ^. #reversed
                , maxDepth = opts ^. #maxDepth
                , substituters
                , format = Maybe.fromMaybe ExportTree (opts ^. #format)
                }

    let httpManagerSettings =
            HttpTls.tlsManagerSettings
                { managerConnCount = numMaxJobs `min` 24
                }
    httpManager <- liftIO $ Http.newManager httpManagerSettings

    derivationCache <- liftIO $ atomically StmMap.new

    let env =
            AppEnvironment
                { numMaxJobs
                , httpManager
                , derivationCache
                , binaryCacheServers = optsDefault ^. #substituters
                }

    pure (env, optsDefault)

readSubstitutersFromNixConf :: (MonadCatch m, MonadIO m) => m (NonEmpty URI)
readSubstitutersFromNixConf = do
    cusotmSubstituters <-
        Directory.doesFileExist "/etc/nix/nix.custom.conf" >>= \case
            True -> readSubstitutersFromFile "/etc/nix/nix.custom.conf"
            False -> pure Nothing

    maybeSubstituters <- case cusotmSubstituters of
        Just substituters -> pure $ Just substituters
        Nothing -> readSubstitutersFromFile "/etc/nix/nix.conf"

    case maybeSubstituters of
        Just res -> pure res
        Nothing -> throwAppErrorText "no substituter URLs from /etc/nix/nix.conf"

readSubstitutersFromFile :: (MonadCatch m, MonadIO m) => FilePath -> m (Maybe (NonEmpty URI))
readSubstitutersFromFile path = do
    content <- checkpointAppError ("could not read content from " <> Text.pack path) $ do
        bytes <- rethrowAsAppError @IOException $ liftIO $ Bytes.readFile path
        throwEitherAsAppError $ TextEncoding.decodeUtf8' bytes

    checkpointAppError ("could not get valid substituters from " <> Text.pack path) $ do
        let values =
                content
                    & Text.lines
                    & Maybe.mapMaybe (Text.stripPrefix "substituters")
                    & concatMap (Text.words . Text.dropWhile (\c -> c == ' ' || c == '='))

        case NonEmpty.nonEmpty values of
            Just s -> case traverse (Uri.parseAbsoluteURI . Text.unpack) s of
                Just substituters -> pure $ Just substituters
                Nothing -> throwAppErrorText "substituter URLs are invalid"
            Nothing -> pure Nothing
