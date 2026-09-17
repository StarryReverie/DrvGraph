module Main (main) where

import Control.Applicative (many, (<**>))
import Control.Exception.Safe (IOException, MonadCatch)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Bifunctor (first)
import Data.Foldable (fold)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Network.URI (URI)
import Network.URI qualified as Uri
import Options.Applicative (Parser, ParserInfo)
import Options.Applicative qualified as Optparse
import System.Exit (ExitCode (..))
import System.FilePath qualified as Path
import System.Process qualified as Process

import DrvGraph.Application (appMain)
import DrvGraph.Application.Argument (AppArguments (..), AppOptions (..))
import DrvGraph.Core.Error (AppEither, addAppErrorContext, appError, renderAppError, rethrowAsAppError, throwAppEither, throwAppErrorText, tryAppError)
import DrvGraph.Core.Model.DerivingPath (DerivingPath)
import DrvGraph.Core.Model.DerivingPath qualified as DerivingPath

data CliOptions = CliOptions
    { showExisted :: Bool
    , showVisited :: Bool
    , showFile :: Bool
    , substituters :: Maybe (NonEmpty URI)
    , useNix2 :: Bool
    , useNix3 :: Bool
    , outName :: Maybe Text
    , rawArguments :: [String]
    }

main :: IO ()
main = Optparse.execParser allOptions >>= appMainCli

appMainCli :: CliOptions -> IO ()
appMainCli cli = do
    res <- tryAppError (resolveArguments cli)
    case res of
        Left err -> TextIO.putStrLn $ renderAppError err
        Right args -> appMain args (toAppOptions cli)

toAppOptions :: CliOptions -> AppOptions
toAppOptions CliOptions{showExisted, showVisited, showFile, substituters} = AppOptions{..}

resolveArguments :: (MonadCatch m, MonadIO m) => CliOptions -> m AppArguments
resolveArguments cli@CliOptions{useNix2, useNix3} =
    case (useNix2, useNix3) of
        (True, True) -> throwAppErrorText "cannot use -2/--nix2 and -3/--nix3 together"
        (False, False) -> resolveFromInput cli
        (True, False) -> resolveFromNix2 cli
        (False, True) -> resolveFromNix3 cli

resolveFromInput :: (MonadCatch m, MonadIO m) => CliOptions -> m AppArguments
resolveFromInput CliOptions{rawArguments, outName} = do
    raw <- case rawArguments of
        (argument : _) -> pure (Text.pack argument)
        [] -> do
            content <- liftIO TextIO.getContents
            case filter (not . Text.null . Text.strip) (Text.lines content) of
                (line : _) -> pure (Text.strip line)
                [] -> throwAppErrorText "no deriving path provided as argument or on stdin"
    (storeDir, drvPath, parsedOutName) <- throwAppEither (parseArgument raw)
    pure AppArguments{storeDir, drvPath, outName = resolveOutName outName parsedOutName}

resolveFromNix2 :: (MonadCatch m, MonadIO m) => CliOptions -> m AppArguments
resolveFromNix2 CliOptions{rawArguments, outName} = do
    (exitCode, output, errOutput) <- runProcess "nix-instantiate" rawArguments
    case exitCode of
        ExitFailure _ -> throwAppErrorText $ "nix-instantiate failed: " <> Text.strip errOutput
        ExitSuccess -> do
            raw <- case filter (not . Text.null . Text.strip) (Text.lines output) of
                (line : _) -> pure (Text.strip line)
                [] -> throwAppErrorText "nix-instantiate produced no deriving path"
            (storeDir, drvPath, parsedOutName) <- throwAppEither (parseArgument raw)
            pure AppArguments{storeDir, drvPath, outName = resolveOutName outName parsedOutName}

resolveFromNix3 :: (MonadCatch m, MonadIO m) => CliOptions -> m AppArguments
resolveFromNix3 CliOptions{rawArguments, outName} = do
    let applyExpr = "drv: \"${drv.drvPath}^${drv.outputName}\""
    (exitCode, output, errOutput) <-
        runProcess "nix" (["eval"] <> rawArguments <> ["--apply", applyExpr, "--raw"])
    case exitCode of
        ExitFailure _ -> throwAppErrorText $ "nix eval failed: " <> Text.strip errOutput
        ExitSuccess -> do
            (storeDir, drvPath, parsedOutName) <- throwAppEither . parseArgument $ Text.strip output
            pure AppArguments{storeDir, drvPath, outName = resolveOutName outName parsedOutName}

-- | Parse @\<storeDir>/\<drvPath>[^\<outName>]@.
parseArgument :: Text -> AppEither (FilePath, DerivingPath, Maybe Text)
parseArgument raw = do
    let (pathPart, outPart) = Text.breakOn "^" raw
    let pathText = Text.unpack (Text.strip pathPart)
    unless (Path.isAbsolute pathText) $
        Left $
            appError ("deriving path is not absolute: " <> Text.pack pathText)
    drvPath <-
        first (addAppErrorContext ("could not parse deriving path " <> Text.pack pathText)) $
            DerivingPath.fromText (Text.pack (Path.takeFileName pathText))
    let parsedOutName = case Text.strip (Text.drop 1 outPart) of
            "" -> Nothing
            name -> Just name
    pure (Path.takeDirectory pathText, drvPath, parsedOutName)

-- | Prefer the output name embedded in the input, then @--out-name@, then @out@.
resolveOutName :: Maybe Text -> Maybe Text -> Text
resolveOutName cliOutName = fromMaybe (fromMaybe "out" cliOutName)

runProcess :: (MonadCatch m, MonadIO m) => FilePath -> [String] -> m (ExitCode, Text, Text)
runProcess command arguments = do
    (exitCode, output, errOutput) <-
        rethrowAsAppError @IOException . liftIO $
            Process.readProcessWithExitCode command arguments ""
    pure (exitCode, Text.pack output, Text.pack errOutput)

allOptions :: ParserInfo CliOptions
allOptions =
    Optparse.info (parseCliOptions <**> Optparse.helper) . fold $
        [ Optparse.fullDesc
        , Optparse.header "DrvGraph - Derivation graph visualizer for unbuilt and unsynced Nix packages"
        , Optparse.progDesc "Traverse the derivation graph rooted at a Nix derivation and print the store paths that need to be built or downloaded. You can provide the derivation via CLI argument or stdin manually or pass through options to Nix to get the path"
        , Optparse.forwardOptions
        ]

parseCliOptions :: Parser CliOptions
parseCliOptions = do
    showExisted <- parseShowExisted
    showVisited <- parseShowVisited
    showFile <- parseShowFile
    substituters <- parseSubstituters
    useNix2 <- parseNix2
    useNix3 <- parseNix3
    outName <- parseOutName
    rawArguments <- many (Optparse.strArgument (Optparse.metavar "ARGS..."))
    pure CliOptions{..}

parseShowExisted :: Parser Bool
parseShowExisted =
    Optparse.switch . fold $
        [ Optparse.long "show-existed"
        , Optparse.help "Whether to show entries that already exist in the nix store"
        ]

parseShowVisited :: Parser Bool
parseShowVisited =
    Optparse.switch . fold $
        [ Optparse.long "show-visited"
        , Optparse.help "Whether to show entries that have been printed previously"
        ]

parseShowFile :: Parser Bool
parseShowFile =
    Optparse.switch . fold $
        [ Optparse.long "show-file"
        , Optparse.help "Whether to show full file names instead of hash prefixes"
        ]

parseSubstituters :: Parser (Maybe (NonEmpty URI))
parseSubstituters =
    Optparse.optional . Optparse.option parser . fold $
        [ Optparse.long "substituters"
        , Optparse.help "Substituters (aka. binary cache servers) to use, separated by space. If absent, read from /etc/nix/nix.conf."
        ]
  where
    parser = Optparse.eitherReader parseSomeUrls
    parseSomeUrls raw = (traverse parseUrl . words $ raw) >>= parseNonEmpty
    parseUrl raw = maybe (Left ("invalid URL: " <> raw)) Right (Uri.parseAbsoluteURI raw)
    parseNonEmpty = maybe (Left "require at least one URL") Right . NonEmpty.nonEmpty

parseNix2 :: Parser Bool
parseNix2 =
    Optparse.switch . fold $
        [ Optparse.short '2'
        , Optparse.long "nix2"
        , Optparse.help "Forward the arguments to nix-instantiate and read the deriving path from its output"
        ]

parseNix3 :: Parser Bool
parseNix3 =
    Optparse.switch . fold $
        [ Optparse.short '3'
        , Optparse.long "nix3"
        , Optparse.help "Forward the arguments to nix eval and read the deriving path and output name from its output"
        ]

parseOutName :: Parser (Maybe Text)
parseOutName =
    Optparse.optional . Optparse.strOption . fold $
        [ Optparse.long "out-name"
        , Optparse.metavar "NAME"
        , Optparse.help "Output name to use when it cannot be obtained from the input (default: out)"
        ]
