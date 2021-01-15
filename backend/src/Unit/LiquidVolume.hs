{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeFamilies #-}

module Unit.LiquidVolume where

import Data.Metrology.Poly

data LiquidVolumeDim = LiquidVolumeDim

instance Dimension LiquidVolumeDim

data Liter = Liter

instance Unit Liter where
  type BaseUnit Liter = Canonical
  type DimOfUnit Liter = LiquidVolumeDim

instance Show Liter where
  show _ = "L"

data Gallon = Gallon

instance Unit Gallon where
  type BaseUnit Gallon = Liter
  conversionRatio _ = 0.264_172

instance Show Gallon where
  show _ = "gal"

data Quart = Quart

instance Unit Quart where
  type BaseUnit Quart = Gallon
  conversionRatio _ = 4

instance Show Quart where
  show _ = "qt"

data Pint = Pint

instance Unit Pint where
  type BaseUnit Pint = Gallon
  conversionRatio _ = 8

instance Show Pint where
  show _ = "pt"

data FluidOunces = FluidOunces

instance Unit FluidOunces where
  type BaseUnit FluidOunces = Gallon
  conversionRatio _ = 128

instance Show FluidOunces where
  show _ = "floz"
