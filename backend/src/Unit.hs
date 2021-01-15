{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Unit
  ( module LiquidVolume,
    module DryVolume,
    module Mass,
    module Prefix,
    module Number,
    Amount,
    LiquidVolume,
    DryVolume,
    Mass,
  )
where

import Data.Metrology.Poly
import Unit.DryVolume as DryVolume
import Unit.LiquidVolume as LiquidVolume
import Unit.Mass as Mass
import Unit.Number as Number
import Unit.Prefix as Prefix

type MyLCSU =
  MkLCSU
    '[ (Dimensionless, Number),
       (LiquidVolumeDim, Liter),
       (DryVolumeDim, Cup),
       (MassDim, Gram)
     ]

type Amount = MkQu_DLN Dimensionless MyLCSU Rational

type LiquidVolume = MkQu_DLN LiquidVolumeDim MyLCSU Rational

type DryVolume = MkQu_DLN DryVolumeDim MyLCSU Rational

type Mass = MkQu_DLN MassDim MyLCSU Rational
