{-# LANGUAGE TypeOperators #-}

module Unit.Prefix where

import Data.Metrology.Poly

data Milli = Milli

instance UnitPrefix Milli where
  multiplier _ = 1 / 1000

instance Show Milli where
  show _ = "m"

milli :: unit -> Milli :@ unit
milli = (Milli :@)

data Centi = Centi

instance UnitPrefix Centi where
  multiplier _ = 100

instance Show Centi where
  show _ = "c"

centi :: unit -> Centi :@ unit
centi = (Centi :@)

data Deci = Deci

instance UnitPrefix Deci where
  multiplier _ = 10

instance Show Deci where
  show _ = "d"

deci :: unit -> Deci :@ unit
deci = (Deci :@)

data Kilo = Kilo

instance UnitPrefix Kilo where
  multiplier _ = 1000

instance Show Kilo where
  show _ = "k"

kilo :: unit -> Kilo :@ unit
kilo = (Kilo :@)
