{-# LANGUAGE NoImplicitPrelude #-}

module ProblemSets.PS1 (
    weightAlInAir,
    weightAlInVacuum,
    weightAlInWater
) where

import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.Quantities
import Lectures.Lecture2 (totalMass, weight, buoyancy, densityAir, densityWater)
-- https://ocw.mit.edu/courses/aeronautics-and-astronautics/16-01-unified-engineering-i-ii-iii-iv-fall-2005-spring-2006/assignments/f01_ps08_fall03.pdf

-- Problem 1 is from textbook

-- Problem 2
-- Weight of 1 kg Al (rho = 2.7 g / cm3) in vacuum, air, and water

densityAl :: Density Double 
densityAl = 2700 *~ (kilo gram / cubic meter)

oneKgAl :: Mass Double 
oneKgAl = 1.0 *~ kilo gram

volumeAl :: Volume Double 
volumeAl = oneKgAl / densityAl

weightAlInVacuum :: Force Double 
weightAlInVacuum = weight oneKgAl

weightAlInWater :: Force Double 
weightAlInWater = weightAlInFluid densityWater 

weightAlInAir :: Force Double 
weightAlInAir = weightAlInFluid densityAir

weightAlInFluid :: Density Double -> Force Double
weightAlInFluid densityFluid = weightAlInVacuum - buoyancy densityFluid volumeAl

