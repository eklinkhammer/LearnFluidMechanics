{-# LANGUAGE NoImplicitPrelude #-}

module Lib
    ( 
        someFunc,
        density,
        totalMass
    ) where

import Numeric.Units.Dimensional.SIUnits
import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.Quantities
import Lectures.Lecture2 (totalMass)

density :: (Floating a) => Mass a -> Volume a -> Density a
density m v = m / v

someFunc :: IO ()
someFunc = putStrLn "someFunc"
