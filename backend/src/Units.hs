{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Units where

import Data.Metrology.Poly

data Dozen = Dozen

instance Unit Dozen where
  type BaseUnit Dozen = Number
  conversionRatio _ = 12

data LengthDim = LengthDim

instance Dimension LengthDim

data Meter = Meter

instance Unit Meter where
  type BaseUnit Meter = Canonical
  type DimOfUnit Meter = LengthDim

type VolumeDim = LengthDim :^ Three

data CubicMeter = CubicMeter

instance Unit CubicMeter where
  type BaseUnit CubicMeter = Canonical
  type DimOfUnit CubicMeter = VolumeDim

data Gallon = Gallon

instance Unit Gallon where
  type BaseUnit Gallon = CubicMeter
  conversionRatio _ = 0.003_785_411_784

data Quart = Quart

instance Unit Quart where
  type BaseUnit Quart = Gallon
  conversionRatio _ = 4

data Pint = Pint

instance Unit Pint where
  type BaseUnit Pint = Gallon
  conversionRatio _ = 8

data FluidOunces = FluidOunces

instance Unit FluidOunces where
  type BaseUnit FluidOunces = Gallon
  conversionRatio _ = 128

type MyLCSU = MkLCSU '[(Dimensionless, Number), (LengthDim, Meter), (VolumeDim, CubicMeter)]

type Amount = MkQu_DLN Dimensionless MyLCSU Rational

type Length = MkQu_DLN LengthDim MyLCSU Rational

type Volume = MkQu_DLN VolumeDim MyLCSU Rational

dozen :: Amount
dozen = 1 % Dozen
