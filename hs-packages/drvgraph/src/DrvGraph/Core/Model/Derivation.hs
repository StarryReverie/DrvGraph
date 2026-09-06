module DrvGraph.Core.Model.Derivation
    ( Derivation (..)
    , DerivationOutput (..)
    , OutputHash (..)
    , parse
    ) where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Text.Megaparsec (Parsec, (<?>))
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC

-- | The Nix derivation's data structure.
data Derivation = Derivation
    { drvInputDrvs :: Map FilePath (Set Text)
    , drvInputSrcs :: Set FilePath
    , drvOutputs :: Map Text DerivationOutput
    , drvPlatform :: Text
    , drvBuilder :: FilePath
    , drvArgs :: [Text]
    , drvEnvs :: Map Text Text
    }
    deriving (Eq, Show)

-- | The output field of a derivation.
data DerivationOutput = DerivationOutput
    { outPath :: FilePath
    , outHash :: Maybe OutputHash
    }
    deriving (Eq, Show)

-- | The hash of a derivation's output.
data OutputHash = OutputHash
    { hashAlgo :: Text
    , hashVal :: Text
    }
    deriving (Eq, Show)

type Parser = Parsec Void Text

-- | Parse a derivation from text.
parse :: Parser Derivation
parse = parseDerivation

parseDerivation :: Parser Derivation
parseDerivation = MP.between (MPC.string "Derive(") (MPC.char ')') $ do
    drvOutputs <- parseManyDerivationOutputs
    _ <- MPC.char ','
    drvInputDrvs <- parseManyInputDrvs
    _ <- MPC.char ','
    drvInputSrcs <- makeSetParser parseFilePath
    _ <- MPC.char ','
    drvPlatform <- parseString
    _ <- MPC.char ','
    drvBuilder <- parseFilePath
    _ <- MPC.char ','
    drvArgs <- makeListParser parseString
    _ <- MPC.char ','
    drvEnvs <- makeMapParser (makePairParaser parseString parseString)
    pure Derivation{drvInputDrvs, drvInputSrcs, drvOutputs, drvPlatform, drvBuilder, drvArgs, drvEnvs}

parseManyDerivationOutputs :: Parser (Map Text DerivationOutput)
parseManyDerivationOutputs = makeMapParser parseDerivationOutput

parseDerivationOutput :: Parser (Text, DerivationOutput)
parseDerivationOutput = MP.between (MPC.char '(') (MPC.char ')') $ do
    outName <- parseString <?> "output name"
    _ <- MPC.char ','
    outPath <- parseFilePath <?> "output path"
    _ <- MPC.char ','
    outHash <- do
        hashAlgo <- parseString <?> "output hash algorithm"
        _ <- MPC.char ','
        hashVal <- parseString <?> "output hash"
        if Text.null hashAlgo && Text.null hashVal
            then pure Nothing
            else pure . Just $ OutputHash{hashAlgo, hashVal}
    let out = DerivationOutput{outPath, outHash}
    pure (outName, out)

parseManyInputDrvs :: Parser (Map FilePath (Set Text))
parseManyInputDrvs = makeMapParser parseInputDrv

parseInputDrv :: Parser (FilePath, Set Text)
parseInputDrv = makePairParaser parseFilePath (makeSetParser parseString)

makeListParser :: Parser a -> Parser [a]
makeListParser p = MP.between (MPC.char '[') (MPC.char ']') (p `MP.sepBy` MPC.char ',')

makeMapParser :: (Ord a) => Parser (a, b) -> Parser (Map a b)
makeMapParser p = Map.fromList <$> makeListParser p

makeSetParser :: (Ord a) => Parser a -> Parser (Set a)
makeSetParser p = Set.fromList <$> makeListParser p

makePairParaser :: Parser a -> Parser b -> Parser (a, b)
makePairParaser p1 p2 = MP.between (MPC.char '(') (MPC.char ')') $ do
    r1 <- p1
    _ <- MPC.char ','
    r2 <- p2
    pure (r1, r2)

parseString :: Parser Text
parseString = MPC.char '\"' >> loop
  where
    loop = do
        normalText <- MP.takeWhileP Nothing (not . isQuoteOrBackslash)
        quoteOrBackslash <- MP.satisfy isQuoteOrBackslash
        remaining <- case quoteOrBackslash of
            '\"' -> pure ""
            _ -> do
                nextChar <- MP.anySingle
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
