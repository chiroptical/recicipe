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
  { ingredients :: [Ingredient]
  }
  deriving (Eq, Show)

data WrappedUnit
  = WrappedMass Mass
  | WrappedLiquidVolume LiquidVolume
  | WrappedDryVolume DryVolume
  | WrappedNumber Amount
  deriving (Eq)

-- TODO:
-- - figure out how these show instances work
instance Show WrappedUnit where
  show (WrappedMass _) = "WrappedMass"
  show (WrappedLiquidVolume _) = "WrappedLiquidVolume"
  show (WrappedDryVolume _) = "WrappedDryVolume"
  show (WrappedNumber _) = "WrappedNumber"

data Ingredient = Ingredient
  { ingredientQuantity :: WrappedUnit,
    ingredientName :: Text
  }
  deriving (Eq, Show)

-- TODO:
-- - This should probably be some kind of Either where Left is an error message?
parseUnit :: Parser (Maybe WrappedUnit)
parseUnit = dbg "units" $
  do
    rat <- parseAAn <|> parseAmount
    mMass <- optional parseMass
    mDryVolume <- optional parseDryVolume
    mLiquidVolume <- optional parseLiquidVolume
    pure $ case (mMass, mDryVolume, mLiquidVolume) of
      (Just ma, Nothing, Nothing) -> Just $ WrappedMass (ma rat)
      (Nothing, Just dv, Nothing) -> Just $ WrappedDryVolume (dv rat)
      (Nothing, Nothing, Just lv) -> Just $ WrappedLiquidVolume (lv rat)
      (Nothing, Nothing, Nothing) -> Just $ WrappedNumber (rat M.% M.Number)
      _ -> Nothing

symbol :: Text -> Parser Text
symbol = L.symbol safeSpace

parseRational :: Parser Rational
parseRational = do
  numerator <- lexeme decimal
  symbol "/"
  denominator <- lexeme decimal
  pure $ numerator % denominator

parseRationalFromScientific :: Parser Rational
parseRationalFromScientific = toRational <$> scientific

parseRationalFromDecimal :: Parser Rational
parseRationalFromDecimal = toRational <$> decimal

parseAmount :: Parser Rational
parseAmount = lexeme $ try parseRational <|> try parseRationalFromScientific <|> parseRationalFromDecimal

notBrace :: Parser Char
notBrace = noneOf ("[]" :: String)

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

parseIngredients :: Parser [Maybe Ingredient]
parseIngredients = dbg "ingredients" $ many notBrace >> sepEndBy (brackets parseIngredient) (many notBrace)

parseIngredient :: Parser (Maybe Ingredient)
parseIngredient = dbg "ingredient" $ do
  units <- parseUnit
  ingredient <- Text.strip . Text.pack <$> many notBrace
  pure $ case units of
    Nothing -> Nothing
    Just wrappedUnit -> Just $ Ingredient wrappedUnit ingredient

parseAAn :: Parser Rational
parseAAn = do
  symbol "a" <|> symbol "an"
  pure 1

safeSpace :: Parser ()
safeSpace = L.space space1 empty empty

-- | This will safely wrap tokens by whitespace
lexeme :: Parser a -> Parser a
lexeme = L.lexeme safeSpace

parseMass :: Parser (Rational -> Mass)
parseMass =
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

parseLiquidVolume :: Parser (Rational -> LiquidVolume)
parseLiquidVolume = do
  asum
    [ (M.% Liter) <$ symbol "liter",
      (M.% Liter) <$ symbol "liters",
      (M.% Liter) <$ symbol "L",
      (M.% Gallon) <$ symbol "gallon",
      (M.% Gallon) <$ symbol "gallons",
      (M.% Gallon) <$ symbol "gal",
      (M.% Gallon) <$ symbol "gal.",
      (M.% Quart) <$ symbol "quart",
      (M.% Quart) <$ symbol "quarts",
      (M.% Quart) <$ symbol "qt",
      (M.% Quart) <$ symbol "qt.",
      (M.% Pint) <$ symbol "pint",
      (M.% Pint) <$ symbol "pints",
      (M.% Pint) <$ symbol "pt",
      (M.% Pint) <$ symbol "pt.",
      (M.% FluidOunces) <$ symbol "fluid ounce",
      (M.% FluidOunces) <$ symbol "fluid ounces",
      (M.% FluidOunces) <$ symbol "fl oz",
      (M.% FluidOunces) <$ symbol "floz",
      (M.% FluidOunces) <$ symbol "flozs",
      (M.% FluidOunces) <$ symbol "fl ozs",
      (M.% FluidOunces) <$ symbol "fl. oz.",
      (M.% FluidOunces) <$ symbol "fl. ozs.",
      (M.% milli Liter) <$ symbol "milliliter",
      (M.% milli Liter) <$ symbol "milliliters",
      (M.% milli Liter) <$ symbol "mL"
    ]

parseDryVolume :: Parser (Rational -> DryVolume)
parseDryVolume = do
  asum
    [ (M.% Cup) <$ symbol "cup",
      (M.% Cup) <$ symbol "cups",
      (M.% Tablespoon) <$ symbol "tablespoon",
      (M.% Tablespoon) <$ symbol "tablespoons",
      (M.% Tablespoon) <$ symbol "tbsp",
      (M.% Tablespoon) <$ symbol "tbsp.",
      (M.% Teaspoon) <$ symbol "teaspoon",
      (M.% Teaspoon) <$ symbol "teaspoons",
      (M.% Teaspoon) <$ symbol "tsp",
      (M.% Teaspoon) <$ symbol "tsp."
    ]
