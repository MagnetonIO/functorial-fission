{-|
Module      : FunctorialFission.Reactor
Description : Reactor simulation and power calculations
Copyright   : (c) Matthew Long, 2025
License     : MIT
Maintainer  : matthew@magneton.ai
Stability   : experimental

This module provides tools for reactor physics calculations:

* Power output from fission rate
* Fuel consumption rates
* Simple reactor state simulation
* Energy unit conversions

These calculations connect the abstract categorical framework to
practical engineering quantities.
-}

module FunctorialFission.Reactor
  ( -- * Physical Constants
    mevToJoules
  , joulesToMev
  , avogadro
  , u235MolarMass
  , u238MolarMass
  , pu239MolarMass
    -- * Reactor Parameters
  , ReactorParams(..)
  , defaultReactor
  , pwr
  , bwr
    -- * Reactor State
  , ReactorState(..)
  , initialState
    -- * Power Calculations
  , fissionRate
  , fuelConsumptionRate
  , burnupRate
  , thermalEfficiency
    -- * Simulation
  , stepReactor
  , runSimulation
  , simulateToTime
    -- * Analysis
  , totalEnergyProduced
  , remainingRuntime
  , capacityFactor
  ) where

import FunctorialFission.Core (MeV, Seconds)

-- ============================================================================
-- Physical Constants
-- ============================================================================

-- | Convert MeV to Joules (1 MeV = 1.602 × 10⁻¹³ J)
mevToJoules :: MeV -> Double
mevToJoules mev = mev * 1.602e-13

-- | Convert Joules to MeV
joulesToMev :: Double -> MeV
joulesToMev j = j / 1.602e-13

-- | Avogadro's number (atoms per mole)
avogadro :: Double
avogadro = 6.022e23

-- | Molar mass of U-235 (g/mol)
u235MolarMass :: Double
u235MolarMass = 235.0

-- | Molar mass of U-238 (g/mol)
u238MolarMass :: Double
u238MolarMass = 238.0

-- | Molar mass of Pu-239 (g/mol)
pu239MolarMass :: Double
pu239MolarMass = 239.0

-- | Energy per fission in Joules
energyPerFissionJ :: Double
energyPerFissionJ = mevToJoules 200.0  -- ≈ 3.2 × 10⁻¹¹ J

-- ============================================================================
-- Reactor Parameters
-- ============================================================================

-- | Reactor operating parameters
data ReactorParams = ReactorParams
  { thermalPowerMW :: !Double   -- ^ Thermal power output (MW)
  , electricPowerMW :: !Double  -- ^ Electrical power output (MW)
  , fuelLoadKg     :: !Double   -- ^ Total fissile fuel load (kg)
  , enrichment     :: !Double   -- ^ Fuel enrichment (fraction of U-235)
  , targetBurnup   :: !Double   -- ^ Target burnup (MWd/kg)
  , kEffNominal    :: !Double   -- ^ Nominal k-effective
  } deriving (Show, Eq)

-- | Default reactor parameters (generic PWR-like)
defaultReactor :: ReactorParams
defaultReactor = ReactorParams
  { thermalPowerMW = 3000.0   -- 3 GW thermal
  , electricPowerMW = 1000.0  -- 1 GW electric (33% efficiency)
  , fuelLoadKg = 100.0        -- 100 kg U-235 equivalent
  , enrichment = 0.035        -- 3.5% enriched
  , targetBurnup = 50.0       -- 50 MWd/kg
  , kEffNominal = 1.0         -- Critical
  }

-- | Typical PWR parameters
pwr :: ReactorParams
pwr = ReactorParams
  { thermalPowerMW = 3400.0
  , electricPowerMW = 1100.0
  , fuelLoadKg = 120.0
  , enrichment = 0.045
  , targetBurnup = 55.0
  , kEffNominal = 1.0
  }

-- | Typical BWR parameters
bwr :: ReactorParams
bwr = ReactorParams
  { thermalPowerMW = 3300.0
  , electricPowerMW = 1100.0
  , fuelLoadKg = 140.0
  , enrichment = 0.032
  , targetBurnup = 45.0
  , kEffNominal = 1.0
  }

-- ============================================================================
-- Reactor State
-- ============================================================================

-- | Instantaneous reactor state
data ReactorState = ReactorState
  { reactorTime      :: !Seconds  -- ^ Simulation time (s)
  , currentPowerMW   :: !Double   -- ^ Current thermal power (MW)
  , energyProducedMJ :: !Double   -- ^ Total thermal energy produced (MJ)
  , fuelRemainingKg  :: !Double   -- ^ Remaining fissile fuel (kg)
  , currentBurnup    :: !Double   -- ^ Current burnup (MWd/kg)
  } deriving (Show, Eq)

-- | Create initial reactor state
initialState :: ReactorParams -> ReactorState
initialState params = ReactorState
  { reactorTime = 0.0
  , currentPowerMW = thermalPowerMW params
  , energyProducedMJ = 0.0
  , fuelRemainingKg = fuelLoadKg params
  , currentBurnup = 0.0
  }

-- ============================================================================
-- Power Calculations
-- ============================================================================

-- | Fission rate required for given power (fissions per second)
--
-- P = R × E_fission
-- R = P / E_fission
fissionRate :: Double -> Double
fissionRate powerWatts = powerWatts / energyPerFissionJ

-- | Fuel consumption rate (kg/s) for given power
--
-- Each fission consumes one U-235 atom
-- Rate = (fissions/s × molar_mass) / (avogadro × 1000)
fuelConsumptionRate :: Double -> Double
fuelConsumptionRate powerWatts =
  (fissionRate powerWatts * u235MolarMass) / (avogadro * 1000)

-- | Burnup rate (MWd/kg per second of operation)
burnupRate :: ReactorParams -> Double
burnupRate params =
  thermalPowerMW params / (fuelLoadKg params * secondsPerDay)
  where secondsPerDay = 86400

-- | Calculate thermal efficiency
thermalEfficiency :: ReactorParams -> Double
thermalEfficiency params =
  electricPowerMW params / thermalPowerMW params

-- ============================================================================
-- Simulation
-- ============================================================================

-- | Advance reactor state by time dt
stepReactor :: Seconds -> ReactorParams -> ReactorState -> ReactorState
stepReactor dt params state = ReactorState
  { reactorTime = reactorTime state + dt
  , currentPowerMW = currentPowerMW state  -- Constant power assumption
  , energyProducedMJ = energyProducedMJ state + energy
  , fuelRemainingKg = fuelRemainingKg state - fuelConsumed
  , currentBurnup = newBurnup
  }
  where
    powerW = currentPowerMW state * 1e6  -- Convert MW to W
    energy = powerW * dt / 1e6           -- MJ produced
    fuelConsumed = fuelConsumptionRate powerW * dt
    newBurnup = (energyProducedMJ state + energy) / (1e6 * fuelLoadKg params * secondsPerDay)
    secondsPerDay = 86400

-- | Run simulation for given duration with time step dt
runSimulation :: Seconds -> Seconds -> ReactorParams -> ReactorState -> [ReactorState]
runSimulation duration dt params initial
  | reactorTime initial >= duration = [initial]
  | fuelRemainingKg initial <= 0 = [initial]
  | otherwise = initial : runSimulation duration dt params (stepReactor dt params initial)

-- | Simulate to a specific end time
simulateToTime :: Seconds -> Seconds -> ReactorParams -> [ReactorState]
simulateToTime endTime dt params = runSimulation endTime dt params (initialState params)

-- ============================================================================
-- Analysis
-- ============================================================================

-- | Total energy produced (MWh)
totalEnergyProduced :: ReactorState -> Double
totalEnergyProduced state = energyProducedMJ state / 3600  -- MJ to MWh

-- | Estimated remaining runtime at current power (seconds)
remainingRuntime :: ReactorParams -> ReactorState -> Seconds
remainingRuntime params state =
  fuelRemainingKg state / fuelConsumptionRate powerW
  where powerW = currentPowerMW state * 1e6

-- | Capacity factor (actual energy / potential energy at full power)
capacityFactor :: ReactorParams -> ReactorState -> Double
capacityFactor params state =
  actualEnergy / potentialEnergy
  where
    actualEnergy = energyProducedMJ state
    potentialEnergy = thermalPowerMW params * reactorTime state
