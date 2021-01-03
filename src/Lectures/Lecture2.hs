{-# LANGUAGE NoImplicitPrelude #-}

module Lectures.Lecture2
(
    totalMass
) where

import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.Quantities
-- Hydrostatic Equation
--   Consider a fluid element in a pressure gradient in y at rest
--   F = ma, so the pressure must equal gravity

netForceOnObjectAtRest :: (Floating a) => Force a
netForceOnObjectAtRest = 0 *~ newton

netForceOnObjectAtInFluid :: (Floating a) 
    => Force a -- Net Pressure
    -> Force a -- Gravity
    -> Force a
netForceOnObjectAtInFluid pressure gravity = pressure - gravity

-- Pressure exists at all levels of a fluid. The net pressure of a fluid is the difference between the pressure
--   at the bottom of the object and the pressure at the top.
-- TODO - verify this conceptual understanding
-- Newton's 3rd law balances out the forces pushing down
-- The fluid itself is not accelerating -> the pressure resists the weight of the fluid. 
pressureOnObjectInFluid :: (Floating a)
    => Pressure a -- Pressure at a level y = y1 of fluid = p dA
    -> Pressure a -- Pressure at level y = y2 = p dA + (dp / dy)dy dA = (p + (dp/dy)dy)dA  
    -> Pressure a -- p dA - ( (p + (dp/dy)dy)dA ) = -(dp/dy)dydA
pressureOnObjectInFluid pressureBottom pressureTop = pressureBottom - pressureTop
-- Note, bottom and top refer to the direction of the pressure. So, pressureBottom would be the shorter distance below water

-- Assuming uniform density of the object, total mass is just rho * volume
totalMass :: (Floating a) => Density a -> Volume a -> Mass a
totalMass d v = d * v

g :: (Floating a) => Acceleration a
g = 9.81 *~ (meter / (second * second))

-- Total force (weight) due to gravity is mg
weight :: (Floating a) => Mass a -> Force a
weight m = force m g

-- f = ma
force :: (Floating a) => Mass a -> Acceleration a -> Force a
force m a = m * a

-- f = ma = 0
-- pressure - gravity = 0
--  -(dp/dy)dydA - rho * g * dV = 0
--  -(dp/dy)*dy*dx*dz = rho * g * dx * dy * dz
--  -(dp/dy) = rho * g
--  Differential Form of Hydrostatic Equation
--    dp = - rho * g * dy

-- Assuming that rho is constant over the area of interest, we can integrate over y
-- p(y) = p(y_0) - rho * g * y
-- The constant of integration is the pressure at y = 0
hydrostaticPressure :: (Floating a) => Pressure a -> Density a -> Acceleration a -> Length a -> Pressure a
hydrostaticPressure fluidPressureAtLength0 densityObjectInFluid a length = fluidPressureAtLength0 - densityObjectInFluid * length * a

-- A manometer is a horseshow shaped tube used to measure pressure
--    p1    p2
--    v_    v_
--    | |   |_| -- difference in pressure causes a difference in height
--    | |___| |
--    |_______|
--        p0 at bottom

-- p1 and p2 are both defined by hydrostatic equation above, sharing p0
-- p1 = p0 - rho * g * (h1 - h0)
-- p2 = p0 - rho * g * (h2 - h0)
-- (p1 - p2) = rho * g * (h2 - h1) OR (p2 - p1) = rho * g * (h1 - h2)
-- if p1 = atmosphere, then p2 = p_atm + rho * g * DH

-- Buoyancy
-- Think of object as infitesmal areas with cross sections under constant hydrostatic pressure (across other dimensions)
--   so, dp/dx = dp/dz = 0
-- dF = pdA - (p + dp/dy DH)dA
--  substituting hydrostatic equation, and we have
-- dF = rho * g * dV

-- Archimedes principle => Buoyancy = displaced weight
buoyancy :: (Floating a) => Density a -> Volume a -> Force a
buoyancy rho v = let mass = totalMass rho v in weight mass

densityAir :: Density Double
densityAir = 1.226 *~ (kilo gram / cubic meter)

densityWater :: Density Double
densityWater = 1000 *~ (kilo gram / cubic meter)
