{-# LANGUAGE TypeFamilies #-}

module Unit.Number where

import Data.Metrology.Poly

data Dozen = Dozen

instance Unit Dozen where
  type BaseUnit Dozen = Number
  conversionRatio _ = 12

instance Show Dozen where
  show _ = "doz"
