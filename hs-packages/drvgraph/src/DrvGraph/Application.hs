module DrvGraph.Application
    ( appMain
    ) where

import Control.Exception.Safe (MonadCatch)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Maybe qualified as Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Optics ((^.))
import System.Console.ANSI (ConsoleLayer (..), setSGRCode)
import System.Console.ANSI.Codes (SGR (..))

import DrvGraph.Application.Argument (AppArguments (..), AppOptions (..), AppOptionsWithDefault)
import DrvGraph.Application.Execution (App, runApp)
import DrvGraph.Application.Initialization (appInit)
import DrvGraph.Core.Error (checkpointAppError, renderAppError, throwAppErrorText, tryAppError)
import DrvGraph.Core.PrettyPrint (EntryLine (..), EntryLineColor (..), ToLinesOptions (..), treeToLines)
import DrvGraph.Core.TreeRepresentation (TreeRepresentationOptions (..), depGraphToTreeRepresentation)
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
        walk storeDir drvPath outName

    let treeOpts =
            TreeRepresentationOptions
                { includeExisted = optsDefault ^. #showExisted
                , includeVisited = optsDefault ^. #showVisited
                , maxDepth = optsDefault ^. #maxDepth
                }
    tree <- case depGraphToTreeRepresentation treeOpts depGraph rootObjPath of
        Just tree -> pure tree
        Nothing -> throwAppErrorText "no valid tree display"

    let toLinesOpts =
            ToLinesOptions
                { showFile = optsDefault ^. #showFile
                , reversed = optsDefault ^. #reversed
                }
    let ls = renderEntryLine (optsDefault ^. #reversed) <$> treeToLines toLinesOpts tree
    liftIO $ TextIO.putStrLn $ Text.unlines ls

    pure ()

renderEntryLine :: Bool -> EntryLine -> Text
renderEntryLine reversed EntryLine{isSubtreeLastChild, content} =
    Text.concat (renderAncestor <$> reverse (drop 1 isSubtreeLastChild))
        <> maybe "" renderConnector (Maybe.listToMaybe isSubtreeLastChild)
        <> Text.intercalate " " (renderChunk <$> content)
  where
    renderAncestor True = "   "
    renderAncestor False = "│  "

    renderConnector True = if reversed then "┌─ " else "└─ "
    renderConnector False = "├─ "

    renderChunk :: (Text, EntryLineColor) -> Text
    renderChunk (chunk, EntryLineColor{color, intensity}) =
        Text.pack (setSGRCode [SetColor Foreground intensity color])
            <> chunk
            <> Text.pack (setSGRCode [Reset])
