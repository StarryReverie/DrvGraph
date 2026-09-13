module DrvGraph.Application.Execution.CapDerivation
    ( loadDerivationImpl
    , queryDeriverImpl
    ) where

import Control.Exception (IOException, try)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString qualified as Bytes
import Data.List qualified as List
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextE
import System.FilePath ((</>))
import System.FilePath qualified as FP
import System.Process qualified as Process
import Text.Megaparsec qualified as MP

import DrvGraph.Core.Error (AppExceptT, appError, exceptionToAppError, liftErr, withErrContext)
import DrvGraph.Core.Model.Derivation (Derivation)
import DrvGraph.Core.Model.Derivation qualified as Derivation
import DrvGraph.Core.Model.DerivingPath (DerivingPath)
import DrvGraph.Core.Model.DerivingPath qualified as DerivingPath
import DrvGraph.Core.Model.StoreObjectPath (StoreObjectPath)
import DrvGraph.Core.Model.StoreObjectPath qualified as StoreObjectPath

loadDerivationImpl
    :: (MonadIO m)
    => FilePath -> DerivingPath -> AppExceptT m Derivation
loadDerivationImpl storeDir drvPath = do
    let path = storeDir </> DerivingPath.toFilePath drvPath

    content <- withErrContext ("could not read content from file " <> Text.pack path) $ do
        bytesRes <- liftIO $ try (Bytes.readFile path)
        bytes <- case bytesRes of
            Left (ex :: IOException) -> throwError $ exceptionToAppError ex
            Right bytes -> pure bytes
        case TextE.decodeUtf8' bytes of
            Left ex -> throwError $ exceptionToAppError ex
            Right content -> pure content

    withErrContext ("could not parse content of file " <> Text.pack path) $ do
        case MP.runParser Derivation.parse "" content of
            Left err -> throwError $ appError . Text.pack . MP.errorBundlePretty $ err
            Right drv -> pure drv

queryDeriverImpl
    :: (MonadIO m)
    => FilePath -> StoreObjectPath -> AppExceptT m (Maybe DerivingPath)
queryDeriverImpl storeDir objPath = do
    let path = storeDir </> StoreObjectPath.toFilePath objPath

    output <- withErrContext ("nix-store command failed for deriver query of " <> Text.pack path) $ do
        let args = ["--query", "--deriver", path]
        outputRes <- liftIO $ try (Process.readProcess "nix-store" args "")
        output <- case outputRes of
            Left (ex :: IOException) -> throwError $ exceptionToAppError ex
            Right output -> pure output
        pure output

    withErrContext ("got invalid result of deriver query of " <> Text.pack path) $ do
        if List.isPrefixOf "unknown-deriver" output
            then pure Nothing
            else do
                let raw = Text.pack $ FP.takeFileName output
                liftErr $ Just <$> DerivingPath.fromText raw
