module DrvGraph.Core.Model.Derivation
    ( Derivation (..)
    , DerivationOutput (..)
    , OutputHash (..)
    , parse
    , existsOutput
    ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Optics ((^.))
import Optics.TH (makeFieldLabelsNoPrefix)
import System.FilePath qualified as Path
import Text.Megaparsec (Parsec, (<?>))
import Text.Megaparsec qualified as Megaparsec
import Text.Megaparsec.Char qualified as MegaparsecChar

import DrvGraph.Core.Model.DerivingPath (DerivingPath)
import DrvGraph.Core.Model.DerivingPath qualified as DerivingPath
import DrvGraph.Core.Model.StoreObjectPath (StoreObjectPath)
import DrvGraph.Core.Model.StoreObjectPath qualified as StoreObjectPath

-- | The Nix derivation's data structure.
data Derivation = Derivation
    { inputDrvs :: Map DerivingPath (Set Text)
    , inputSrcs :: Set StoreObjectPath
    , outputs :: Map Text DerivationOutput
    , platform :: Text
    , builder :: Text
    , args :: [Text]
    , envs :: Map Text Text
    }
    deriving (Eq, Show)

-- | The output field of a derivation.
data DerivationOutput = DerivationOutput
    { path :: StoreObjectPath
    , hash :: Maybe OutputHash
    }
    deriving (Eq, Show)

-- | The hash of a derivation's output.
data OutputHash = OutputHash
    { algo :: Text
    , val :: Text
    }
    deriving (Eq, Show)

makeFieldLabelsNoPrefix ''Derivation
makeFieldLabelsNoPrefix ''DerivationOutput
makeFieldLabelsNoPrefix ''OutputHash

type Parser = Parsec Void Text

-- | Parse a derivation from text.
parse :: Parser Derivation
parse = parseDerivation

parseDerivation :: Parser Derivation
parseDerivation = Megaparsec.between (MegaparsecChar.string "Derive(") (MegaparsecChar.char ')') $ do
    outputs <- parseManyDerivationOutputs <?> "derivation outputs"
    _ <- MegaparsecChar.char ','
    inputDrvs <- parseManyInputDrvs <?> "derivation input derivations"
    _ <- MegaparsecChar.char ','
    inputSrcs <- makeSetParser parseStoreObjectPath <?> "derivation input sources"
    _ <- MegaparsecChar.char ','
    platform <- parseString <?> "derivation platform"
    _ <- MegaparsecChar.char ','
    builder <- parseString <?> "derivation builder"
    _ <- MegaparsecChar.char ','
    args <- makeListParser parseString <?> "derivation arguments"
    _ <- MegaparsecChar.char ','
    envs <- makeMapParser (makePairParaser parseString parseString) <?> "derivation environments"
    pure Derivation{inputDrvs, inputSrcs, outputs, platform, builder, args, envs}

parseManyDerivationOutputs :: Parser (Map Text DerivationOutput)
parseManyDerivationOutputs = makeMapParser parseDerivationOutput

parseDerivationOutput :: Parser (Text, DerivationOutput)
parseDerivationOutput = Megaparsec.between (MegaparsecChar.char '(') (MegaparsecChar.char ')') $ do
    outName <- parseString <?> "output name"
    _ <- MegaparsecChar.char ','
    path <- parseStoreObjectPath <?> "output path"
    _ <- MegaparsecChar.char ','
    hash <- do
        algo <- parseString <?> "output hash algorithm"
        _ <- MegaparsecChar.char ','
        val <- parseString <?> "output hash"
        if Text.null algo && Text.null val
            then pure Nothing
            else pure . Just $ OutputHash{algo, val}
    let out = DerivationOutput{path, hash}
    pure (outName, out)

parseManyInputDrvs :: Parser (Map DerivingPath (Set Text))
parseManyInputDrvs = makeMapParser parseInputDrv

parseInputDrv :: Parser (DerivingPath, Set Text)
parseInputDrv = makePairParaser parseDerivingPath (makeSetParser parseString)

parseDerivingPath :: Parser DerivingPath
parseDerivingPath = do
    filePath <- Path.takeFileName <$> parseFilePath
    case DerivingPath.fromText . Text.pack $ filePath of
        Left _ -> fail "deriving path is invalid"
        Right dp -> pure dp

parseStoreObjectPath :: Parser StoreObjectPath
parseStoreObjectPath = do
    filePath <- Path.takeFileName <$> parseFilePath
    case StoreObjectPath.fromText . Text.pack $ filePath of
        Left _ -> fail "store object path is invalid"
        Right dp -> pure dp

makeListParser :: Parser a -> Parser [a]
makeListParser p =
    Megaparsec.between
        (MegaparsecChar.char '[')
        (MegaparsecChar.char ']')
        (p `Megaparsec.sepBy` MegaparsecChar.char ',')

makeMapParser :: (Ord a) => Parser (a, b) -> Parser (Map a b)
makeMapParser p = Map.fromList <$> makeListParser p

makeSetParser :: (Ord a) => Parser a -> Parser (Set a)
makeSetParser p = Set.fromList <$> makeListParser p

makePairParaser :: Parser a -> Parser b -> Parser (a, b)
makePairParaser p1 p2 = Megaparsec.between (MegaparsecChar.char '(') (MegaparsecChar.char ')') $ do
    r1 <- p1
    _ <- MegaparsecChar.char ','
    r2 <- p2
    pure (r1, r2)

parseString :: Parser Text
parseString = MegaparsecChar.char '\"' >> loop
  where
    loop = do
        normalText <- Megaparsec.takeWhileP Nothing (not . isQuoteOrBackslash)
        quoteOrBackslash <- Megaparsec.satisfy isQuoteOrBackslash
        remaining <- case quoteOrBackslash of
            '\"' -> pure ""
            _ -> do
                nextChar <- Megaparsec.anySingle
                unescaped <- case nextChar of
                    'n' -> pure '\n'
                    't' -> pure '\t'
                    'r' -> pure '\r'
                    other -> pure other
                Text.cons unescaped <$> loop
        pure $ normalText <> remaining

    isQuoteOrBackslash c = c == '\"' || c == '\\'

parseFilePath :: Parser FilePath
parseFilePath = do
    unchecked <- parseString
    checked <- case Text.uncons unchecked of
        Just (first, _) | first == '/' -> pure unchecked
        _ -> fail "file path should not be empty and starts with '/'"
    pure $ Text.unpack checked

existsOutput :: StoreObjectPath -> Derivation -> Bool
existsOutput objPath drv = any (\out -> out ^. #path == objPath) (drv ^. #outputs)
