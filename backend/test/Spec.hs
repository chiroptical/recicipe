{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import Parser
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

partOne :: Text
partOne = "Put [1/2 quart double cream], [8.8 oz mascarpone], [75 ml marsala] and [5 tbsp golden caster sugar] in a large bowl."

partTwo :: Text
partTwo = "Whisk until the cream and mascarpone have completely combined and have the consistency of thickly whipped cream."

partThree :: Text
partThree = "Pour [300 ml coffee] into a shallow dish. Dip in a few of the [175g package of sponge fingers] at a time, turning for a few seconds until they are nicely soaked, but not soggy.  Layer these in a dish until you’ve used half the sponge fingers, then spread over half of the creamy mixture."

partFour :: Text
partFour = "Using the coarse side of a grater, grate over most of the [25 g dark chocolate].  Then repeat the layers (you should use up all the coffee), finishing with the creamy layer."

partFive :: Text
partFive = "Cover and chill for a few hours or overnight. Will keep in the fridge for up to two days."

partSix :: Text
partSix = "To serve, dust with the [2 tsp cocoa powder] and grate over the remainder of the chocolate."

main :: IO ()
main = hspec $ do
  describe "parser" $ do
    it "will parse 4 entries" $ do
      parse parseEntries "" partOne `parseSatisfies` ((== 4) . length . entries)
