{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeFamilies #-}

module Unit.Mass where

import Data.Metrology.Poly

data MassDim = MassDim

instance Dimension MassDim

data Gram = Gram

instance Unit Gram where
  type BaseUnit Gram = Canonical
  type DimOfUnit Gram = MassDim

instance Show Gram where
  show _ = "g"

data Pound = Pound

instance Unit Pound where
  type BaseUnit Pound = Gram
  conversionRatio _ = 0.002_204_62

instance Show Pound where
  show _ = "lbs"

data Ounce = Ounce

instance Unit Ounce where
  type BaseUnit Ounce = Pound
  conversionRatio _ = 16

instance Show Ounce where
  show _ = "oz"
