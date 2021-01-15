{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Metrology.Poly as M
import Data.Ratio ((%))
import Data.Text (Text)
import Parser
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Unit

ingredientsLengthShouldBe :: Text -> Int -> Expectation
ingredientsLengthShouldBe text len =
  parse parseIngredients "" text `parseSatisfies` ((== len) . length . ingredients)

ingredientShouldBe :: Text -> Ingredient -> Expectation
ingredientShouldBe text = parseSatisfies (parse parseIngredient "" text) . (==)

main :: IO ()
main = hspec $ do
  describe "parses a or an" $ do
    it "can parse a" $
      parse parseAAn "" "a" `parseSatisfies` (== 1)
    it "can't parse b" $
      parse parseAAn "" `shouldFailOn` "b"
  describe "parses rational numbers" $ do
    it "can parse 1/2" $
      parseSatisfies (parse parseRational "" "1 / 2") (== 1 % 2)
  describe "parses correct number of ingredients" $ do
    it "will parse 4 ingredients" $
      caseZero `ingredientsLengthShouldBe` 4
    it "will parse 0 ingredients" $
      caseOne `ingredientsLengthShouldBe` 0
    it "will parse 2 ingredients" $
      caseTwo `ingredientsLengthShouldBe` 2
    it "will parse 1 ingredient" $
      caseThree `ingredientsLengthShouldBe` 1
    it "will parse 0 ingredients" $
      caseFour `ingredientsLengthShouldBe` 0
    it "will parse 1 ingredient" $
      caseFive `ingredientsLengthShouldBe` 1
  describe "will parse correct ingredient" $ do
    it "will parse" $
      "1/2 quart double cream" `ingredientShouldBe` Ingredient (1 % 2) "quart" "double cream"
    it "will parse" $
      "8.8 oz mascarpone" `ingredientShouldBe` Ingredient (44 % 5) "oz" "mascarpone"
    it "will parse" $
      "12 eggs" `ingredientShouldBe` Ingredient (12 % 1) "" "eggs"
    it "will parse" $
      "175g package of sponge fingers" `ingredientShouldBe` Ingredient (175 % 1) "g" "package of sponge fingers"
  describe "parsing units" $ do
    it "will parse mass" $
      ((M.# Ounce) <$> parse parseMass "" "12 ounce") `parseSatisfies` (== 12)
    it "will parse mass" $
      ((M.# Ounce) <$> parse parseMass "" "2.0 ounce") `parseSatisfies` (== 2 % 1)
    it "will parse mass" $
      ((M.# Ounce) <$> parse parseMass "" "1/2 ounce") `parseSatisfies` (== 1 % 2)

caseZero :: Text
caseZero = "Put [1/2 quart double cream], [8.8 oz mascarpone], [75 ml marsala] and [5 tbsp golden caster sugar] in a large bowl."

caseOne :: Text
caseOne = "Whisk until the cream and mascarpone have completely combined and have the consistency of thickly whipped cream."

caseTwo :: Text
caseTwo = "Pour [300 ml coffee] into a shallow dish. Dip in a few of the [175g package of sponge fingers] at a time, turning for a few seconds until they are nicely soaked, but not soggy.  Layer these in a dish until you’ve used half the sponge fingers, then spread over half of the creamy mixture."

caseThree :: Text
caseThree = "Using the coarse side of a grater, grate over most of the [25 g dark chocolate].  Then repeat the layers (you should use up all the coffee), finishing with the creamy layer."

caseFour :: Text
caseFour = "Cover and chill for a few hours or overnight. Will keep in the fridge for up to two days."

caseFive :: Text
caseFive = "To serve, dust with the [2 tsp cocoa powder] and grate over the remainder of the chocolate."
