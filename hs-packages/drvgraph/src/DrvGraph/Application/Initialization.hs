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
import Network.HTTP.Client qualified as Http
import Network.HTTP.Client.TLS qualified as HttpTls
import Network.URI (URI)
import Network.URI qualified as Uri
import Optics ((^.))
import StmContainers.Map qualified as StmMap

import DrvGraph.Application.Argument (AppOptions (..), AppOptionsWithDefault (..))
import DrvGraph.Application.Execution.Environment (AppEnvironment (..))
import DrvGraph.Core.Error (checkpointAppError, rethrowAsAppError, throwAppErrorText, throwEitherAsAppError)

appInit :: (MonadCatch m, MonadIO m) => AppOptions -> m (AppEnvironment, AppOptionsWithDefault)
appInit opts = do
    substituters <- maybe readSubstitutersFromNixConf pure (opts ^. #substituters)

    let optsDefault =
            AppOptionsWithDefault
                { showExisted = opts ^. #showExisted
                , showVisited = opts ^. #showVisited
                , showFile = opts ^. #showFile
                , substituters
                }

    httpManager <- liftIO $ Http.newManager HttpTls.tlsManagerSettings
    derivationCache <- liftIO $ atomically $ StmMap.new

    let env =
            AppEnvironment
                { httpManager
                , derivationCache
                , binaryCacheServers = optsDefault ^. #substituters
                }

    pure (env, optsDefault)

readSubstitutersFromNixConf :: (MonadCatch m, MonadIO m) => m (NonEmpty URI)
readSubstitutersFromNixConf = do
    content <- checkpointAppError "could not read content from /etc/nix/nix.conf" $ do
        bytes <- rethrowAsAppError @IOException $ liftIO $ Bytes.readFile "/etc/nix/nix.conf"
        throwEitherAsAppError $ TextEncoding.decodeUtf8' bytes

    checkpointAppError "could not get valid substituters from /etc/nix/nix.conf" $ do
        let values =
                content
                    & Text.lines
                    & Maybe.mapMaybe (Text.stripPrefix "substituters")
                    & concatMap (Text.words . Text.dropWhile (\c -> c == ' ' || c == '='))

        maybeSubstituters <- case NonEmpty.nonEmpty values of
            Just maybeSubstituters -> pure maybeSubstituters
            Nothing -> throwAppErrorText "substituter list is empty"

        case traverse (Uri.parseAbsoluteURI . Text.unpack) maybeSubstituters of
            Just substituters -> pure substituters
            Nothing -> throwAppErrorText "substituter URLs are invalid"
