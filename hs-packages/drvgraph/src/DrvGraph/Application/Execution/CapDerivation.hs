module DrvGraph.Application.Execution.CapDerivation
    ( loadDerivationImpl
    ) where

import Control.Exception.Safe (IOException, MonadCatch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString qualified as Bytes
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import System.FilePath ((</>))
import Text.Megaparsec qualified as Megaparsec

import DrvGraph.Core.Error (checkpointAppError, rethrowAsAppError, throwAppErrorText, throwEitherAsAppError)
import DrvGraph.Core.Model.Derivation (Derivation)
import DrvGraph.Core.Model.Derivation qualified as Derivation
import DrvGraph.Core.Model.DerivingPath (DerivingPath)
import DrvGraph.Core.Model.DerivingPath qualified as DerivingPath

loadDerivationImpl
    :: (MonadCatch m, MonadIO m)
    => FilePath -> DerivingPath -> m Derivation
loadDerivationImpl storeDir drvPath = do
    let path = storeDir </> DerivingPath.toFilePath drvPath

    content <- checkpointAppError ("could not read content from file " <> Text.pack path) $ do
        bytes <- rethrowAsAppError @IOException $ liftIO $ Bytes.readFile path
        throwEitherAsAppError $ TextEncoding.decodeUtf8' bytes

    checkpointAppError ("could not parse content of file " <> Text.pack path) $ do
        case Megaparsec.runParser Derivation.parse "" content of
            Left err -> throwAppErrorText $ Text.pack . Megaparsec.errorBundlePretty $ err
            Right drv -> pure drv
