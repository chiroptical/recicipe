{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Data.Foldable (asum)
import Data.Maybe (fromMaybe)
import qualified Data.Metrology.Poly as M
import Data.Ratio ((%))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (space1)
import Text.Megaparsec.Char.Lexer (decimal, scientific)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug (dbg)
import Unit

type Parser = Parsec Void Text

newtype Ingredients = Ingredients
  { ingredients :: [Text]
  }
  deriving (Eq, Show)

data Ingredient = Ingredient
  { ingredientAmount :: Rational,
    ingredientUnit :: Text,
    ingredientName :: Text
  }
  deriving (Eq, Show)

parseUnit :: Parser Text
parseUnit =
  lexeme $
    chunk "quart"
      <|> chunk "oz"
      <|> chunk "gram"
      <|> chunk "g"

noParseTrailingUnit :: Parser (Maybe Text)
noParseTrailingUnit = lookAhead (optional parseUnit)

symbol :: Text -> Parser Text
symbol = L.symbol safeSpace

parseRational :: Parser Rational
parseRational = do
  numerator <- lexeme decimal
  symbol "/"
  denominator <- lexeme decimal
  noParseTrailingUnit
  pure $ numerator % denominator

parseRationalFromScientific :: Parser Rational
parseRationalFromScientific = toRational <$> scientific <* noParseTrailingUnit

parseRationalFromDecimal :: Parser Rational
parseRationalFromDecimal = toRational <$> decimal <* noParseTrailingUnit

parseAmount :: Parser Rational
parseAmount = lexeme $ try parseRational <|> try parseRationalFromScientific <|> parseRationalFromDecimal

notBrace :: Parser Char
notBrace = noneOf ("[]" :: String)

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

parseTextFromBrackets :: Parser Text
parseTextFromBrackets = dbg "textFromBrackets" . try $ do
  text <- brackets $ many (anySingleBut ']')
  return . Text.pack $ text

parseIngredients :: Parser Ingredients
parseIngredients = dbg "ingredients" . try $ do
  entries <- many notBrace *> sepEndBy parseTextFromBrackets (many notBrace)
  return . Ingredients $ entries

parseIngredient :: Parser Ingredient
parseIngredient = do
  amount <- fromMaybe 1 <$> optional parseAmount
  units <- fromMaybe "" <$> optional parseUnit
  ingredient <- Text.strip <$> takeRest
  pure $ Ingredient amount units ingredient

parseAAn :: Parser Rational
parseAAn = do
  symbol "a" <|> symbol "an"
  pure 1

safeSpace :: Parser ()
safeSpace = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme safeSpace

parseOunce :: Parser (Rational -> Mass)
parseOunce = do
  -- if "oz"
  pure (M.% Ounce)

parseGram :: Parser (Rational -> Mass)
parseGram = pure (M.% Gram)

parseMass :: Parser Mass
parseMass = do
  rat <- parseAAn <|> parseAmount
  toUnit <-
    asum
      [ (M.% Ounce) <$ symbol "oz",
        (M.% Ounce) <$ symbol "oz.",
        (M.% Ounce) <$ symbol "ounce",
        (M.% Ounce) <$ symbol "ounces",
        (M.% Gram) <$ symbol "g",
        (M.% Gram) <$ symbol "gram",
        (M.% Gram) <$ symbol "grams",
        (M.% milli Gram) <$ symbol "mg",
        (M.% milli Gram) <$ symbol "milligram",
        (M.% milli Gram) <$ symbol "milligrams",
        (M.% kilo Gram) <$ symbol "kg",
        (M.% kilo Gram) <$ symbol "kilogram",
        (M.% kilo Gram) <$ symbol "kiligrams"
      ]
  pure $ toUnit rat
