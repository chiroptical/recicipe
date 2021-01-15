{-# LANGUAGE TypeFamilies #-}

module Unit.DryVolume where

import Data.Metrology.Poly

data DryVolumeDim = DryVolumeDim

instance Dimension DryVolumeDim

data Cup = Cup

instance Unit Cup where
  type BaseUnit Cup = Canonical
  type DimOfUnit Cup = DryVolumeDim

instance Show Cup where
  show _ = "cups"

data Tablespoon = Tablespoon

instance Unit Tablespoon where
  type BaseUnit Tablespoon = Cup
  conversionRatio _ = 16

instance Show Tablespoon where
  show _ = "tbsp"

data Teaspoon = Teaspoon

instance Unit Teaspoon where
  type BaseUnit Teaspoon = Tablespoon
  conversionRatio _ = 3

instance Show Teaspoon where
  show _ = "tsp"
