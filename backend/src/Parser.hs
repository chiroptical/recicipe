{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (char, space)
import Text.Megaparsec.Char.Lexer (decimal, scientific)
import Text.Megaparsec.Debug (dbg)

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
  chunk "quart"
    <|> chunk "oz"
    <|> chunk "gram"
    <|> chunk "g"

noParseTrailingUnit :: Parser (Maybe Text)
noParseTrailingUnit = lookAhead (optional parseUnit)

parseRational :: Parser Rational
parseRational = do
  numerator <- decimal
  space
  char '/'
  space
  denominator <- decimal
  noParseTrailingUnit
  pure $ numerator % denominator

parseRationalFromScientific :: Parser Rational
parseRationalFromScientific = toRational <$> scientific <* noParseTrailingUnit

parseRationalFromDecimal :: Parser Rational
parseRationalFromDecimal = toRational <$> decimal <* noParseTrailingUnit

parseAmount :: Parser Rational
parseAmount =
  try parseRational <|> try parseRationalFromScientific <|> parseRationalFromDecimal

notBrace :: Parser Char
notBrace = noneOf ("[]" :: String)

brackets :: Parser a -> Parser a
brackets = between (char '[') (char ']')

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
  optional space
  units <- fromMaybe "" <$> optional parseUnit
  space
  ingredient <- Text.strip <$> takeRest
  pure $ Ingredient amount units ingredient
