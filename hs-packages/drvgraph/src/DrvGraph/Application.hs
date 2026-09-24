module DrvGraph.Application
    ( appMain
    ) where

import Control.Exception.Safe (MonadCatch)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text.IO qualified as TextIO
import Optics ((^.))
import System.IO (stderr)

import DrvGraph.Application.Argument (AppArguments (..), AppOptions (..), AppOptionsWithDefault, ExportFormat (ExportJson, ExportTree))
import DrvGraph.Application.Execution (App, runApp)
import DrvGraph.Application.Initialization (appInit)
import DrvGraph.Core.Error (checkpointAppError, renderAppError, throwAppError, tryAppError)
import DrvGraph.Core.Export.Json (exportJsonText)
import DrvGraph.Core.Export.Tree (FlattenTreeOptions (..), ToTreeOptions (..), exportTreeText)
import DrvGraph.Core.Walk (walk)

appMain :: (MonadCatch m, MonadIO m) => AppArguments -> AppOptions -> m ()
appMain args opts = do
    (env, optsDefault) <- appInit opts
    liftIO (runApp (tryAppError (app args optsDefault)) env) >>= \case
        Left err -> liftIO $ TextIO.putStrLn $ renderAppError err
        Right () -> pure ()

app :: AppArguments -> AppOptionsWithDefault -> App ()
app args optsDefault = do
    let AppArguments{storeDir, drvPath, outName} = args

    (depGraph, rootObjPath) <- checkpointAppError "could not traverse nix store" $ do
        liftIO $ TextIO.hPutStrLn stderr "[DrvGraph] Traversing Nix store"
        walk storeDir drvPath outName

    case optsDefault ^. #format of
        ExportTree -> do
            let toTreeOpts =
                    ToTreeOptions
                        { includeExisted = optsDefault ^. #showExisted
                        , includeVisited = optsDefault ^. #showVisited
                        , maxDepth = optsDefault ^. #maxDepth
                        }
            let flattenTreeOpts =
                    FlattenTreeOptions
                        { showFile = optsDefault ^. #showFile
                        , reversed = optsDefault ^. #reversed
                        }
            case exportTreeText toTreeOpts flattenTreeOpts (depGraph, rootObjPath) of
                Left err -> throwAppError err
                Right res -> liftIO $ TextIO.putStrLn res
        ExportJson -> do
            let text = exportJsonText (depGraph, rootObjPath)
            liftIO $ TextIO.putStrLn text

    pure ()
