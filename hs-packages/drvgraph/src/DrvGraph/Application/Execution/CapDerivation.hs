module DrvGraph.Application.Execution.CapDerivation
    ( loadDerivationImpl
    ) where

import Control.Exception (IOException, try)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString qualified as Bytes
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import System.FilePath ((</>))
import Text.Megaparsec qualified as Megaparsec

import DrvGraph.Core.Error (AppExceptT, appError, exceptionToAppError, withErrContext)
import DrvGraph.Core.Model.Derivation (Derivation)
import DrvGraph.Core.Model.Derivation qualified as Derivation
import DrvGraph.Core.Model.DerivingPath (DerivingPath)
import DrvGraph.Core.Model.DerivingPath qualified as DerivingPath

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
        case TextEncoding.decodeUtf8' bytes of
            Left ex -> throwError $ exceptionToAppError ex
            Right content -> pure content

    withErrContext ("could not parse content of file " <> Text.pack path) $ do
        case Megaparsec.runParser Derivation.parse "" content of
            Left err -> throwError $ appError . Text.pack . Megaparsec.errorBundlePretty $ err
            Right drv -> pure drv
