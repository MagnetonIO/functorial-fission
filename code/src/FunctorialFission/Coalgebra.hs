{-|
Module      : FunctorialFission.Coalgebra
Description : Coalgebraic treatment of chain reactions
Copyright   : (c) Matthew Long, 2025
License     : MIT
Maintainer  : matthew@magneton.ai
Stability   : experimental

This module provides a coalgebraic formulation of nuclear chain reactions.

Coalgebras are the categorical dual of algebras, well-suited for modeling
systems with branching or nondeterministic evolution. For chain reactions:

* The branching functor F(X) = (P_fin(X), R+) captures possible outcomes and energy
* Fission coalgebras model nuclear states with their transition structures
* The multiplication factor k emerges naturally from the coalgebraic structure
* Delayed neutrons extend the basic coalgebra with time annotations

This formulation provides a principled framework for understanding chain
reaction dynamics and connects to the theory of systems and corecursion.
-}

{-# LANGUAGE DeriveFunctor #-}

module FunctorialFission.Coalgebra
  ( -- * Branching Functor
    BranchF(..)
  , mapBranch
    -- * Coalgebra Class
  , Coalgebra(..)
    -- * Neutron Population
  , NeutronState(..)
  , mkNeutronState
  , criticalState
    -- * Chain Reaction Parameters
  , ChainParams(..)
  , defaultParams
  , promptParams
    -- * Iteration and Dynamics
  , iterateCoalg
  , totalEnergyAfter
  , populationAfter
  , timeToPopulation
    -- * Delayed Neutrons
  , DelayedNeutron(..)
  , DelayedGroup(..)
  , u235DelayedGroups
    -- * Extended Coalgebra
  , ExtendedBranch(..)
  , effectiveGenerationTime
    -- * Criticality Analysis
  , Criticality(..)
  , criticalityOf
  , reactorPeriod
  ) where

import FunctorialFission.Core (MeV, Seconds)

-- | The branching functor F(X) = (List X, MeV)
--
-- Maps a type X to pairs of:
--   * A list of possible outcomes (finite powerset represented as list)
--   * The energy released in the transition
--
-- This is an endofunctor on Hask suitable for coalgebraic modeling.
data BranchF a = BranchF
  { outcomes      :: [a]   -- ^ Finite set of possible next states
  , energyOutput  :: !MeV  -- ^ Energy released in transition
  } deriving (Show, Eq, Functor)

-- | Map over branch outcomes
mapBranch :: (a -> b) -> BranchF a -> BranchF b
mapBranch = fmap

-- | Typeclass for F-coalgebras
--
-- A coalgebra is a pair (A, α) where α: A → F(A)
-- This captures systems that can "observe" or "unfold" their state.
class Coalgebra f a where
  -- | The structure map α: A → F(A)
  coalg :: a -> f a

-- | Neutron population state for chain reaction modeling
data NeutronState = NeutronState
  { population   :: !Double  -- ^ Number of neutrons (continuous for averaging)
  , generation   :: !Int     -- ^ Generation number
  , elapsedTime  :: !Seconds -- ^ Total elapsed time
  } deriving (Show, Eq)

-- | Create an initial neutron state
mkNeutronState :: Double -> NeutronState
mkNeutronState n = NeutronState n 0 0.0

-- | A critical state with one neutron
criticalState :: NeutronState
criticalState = mkNeutronState 1.0

-- | Parameters for chain reaction dynamics
data ChainParams = ChainParams
  { kEffective       :: !Double   -- ^ Effective multiplication factor
  , generationTime   :: !Seconds  -- ^ Mean generation time (Λ)
  , energyPerFission :: !MeV      -- ^ Energy released per fission
  , neutronsPerFission :: !Double -- ^ Average neutrons per fission (ν)
  } deriving (Show, Eq)

-- | Default parameters (typical thermal reactor)
defaultParams :: ChainParams
defaultParams = ChainParams
  { kEffective       = 1.0     -- Critical
  , generationTime   = 1.0e-4  -- 0.1 ms for thermal neutrons
  , energyPerFission = 200.0   -- MeV
  , neutronsPerFission = 2.4   -- Average for U-235
  }

-- | Parameters for prompt-only dynamics (fast excursion)
promptParams :: ChainParams
promptParams = defaultParams
  { generationTime = 1.0e-8  -- Much faster without delayed neutrons
  }

-- | Coalgebra instance for neutron population dynamics
--
-- The structure map unfolds one generation of the chain reaction.
instance Coalgebra BranchF (NeutronState, ChainParams) where
  coalg (state, params) = BranchF
    { outcomes = [nextState]
    , energyOutput = fissions * energyPerFission params
    }
    where
      k = kEffective params
      dt = generationTime params
      newPop = population state * k
      fissions = population state / neutronsPerFission params
      nextState = (NeutronState
        { population = newPop
        , generation = generation state + 1
        , elapsedTime = elapsedTime state + dt
        }, params)

-- | Iterate the coalgebra n times, collecting all branches
iterateCoalg :: Coalgebra BranchF a => Int -> a -> [BranchF a]
iterateCoalg 0 _ = []
iterateCoalg n x = fx : concatMap (iterateCoalg (n-1)) (outcomes fx)
  where fx = coalg x

-- | Calculate total energy released after n generations
totalEnergyAfter :: Int -> NeutronState -> ChainParams -> MeV
totalEnergyAfter n initial params =
  sum $ map energyOutput $ iterateCoalg n (initial, params)

-- | Population after n generations (analytical formula)
--
-- N_n = N_0 * k^n
populationAfter :: Int -> NeutronState -> ChainParams -> Double
populationAfter n state params =
  population state * (kEffective params ^ n)

-- | Time for population to reach target (analytical formula)
--
-- From N(t) = N_0 * exp((k-1)t/Λ)
-- t = Λ * ln(N_target/N_0) / (k-1)
timeToPopulation :: Double -> NeutronState -> ChainParams -> Maybe Seconds
timeToPopulation target state params
  | k == 1.0  = Nothing  -- Never reaches target if critical
  | k < 1.0 && target > population state = Nothing  -- Subcritical can't grow
  | otherwise = Just $ lambda * log (target / population state) / (k - 1)
  where
    k = kEffective params
    lambda = generationTime params

-- | Delayed neutron data
data DelayedNeutron = DelayedNeutron
  { delayTime     :: !Seconds  -- ^ Mean delay time
  , delayFraction :: !Double   -- ^ Fraction of fission neutrons delayed
  } deriving (Show, Eq)

-- | Delayed neutron group (there are typically 6 groups)
data DelayedGroup = DelayedGroup
  { groupIndex   :: !Int       -- ^ Group number (1-6)
  , halfLife     :: !Seconds   -- ^ Half-life of precursor
  , decayConst   :: !Double    -- ^ Decay constant λ = ln(2)/T_½
  , betaFraction :: !Double    -- ^ Fraction β_i of delayed neutrons
  } deriving (Show, Eq)

-- | Standard 6-group delayed neutron data for U-235
u235DelayedGroups :: [DelayedGroup]
u235DelayedGroups =
  [ DelayedGroup 1 55.72   0.01244 0.000215
  , DelayedGroup 2 22.72   0.0305  0.001424
  , DelayedGroup 3 6.22    0.1114  0.001274
  , DelayedGroup 4 2.30    0.3014  0.002568
  , DelayedGroup 5 0.610   1.136   0.000748
  , DelayedGroup 6 0.230   3.014   0.000273
  ]

-- | Total delayed neutron fraction for U-235
totalBeta :: Double
totalBeta = sum $ map betaFraction u235DelayedGroups  -- ≈ 0.0065

-- | Extended branching structure including delayed neutrons
data ExtendedBranch a = ExtendedBranch
  { promptOutcomes  :: [(a, Double)]    -- ^ (outcome, probability) pairs
  , delayedOutcomes :: [(a, DelayedNeutron)]  -- ^ Delayed contributions
  , branchEnergy    :: !MeV             -- ^ Energy released
  } deriving (Show)

-- | Calculate effective generation time including delayed neutrons
--
-- Λ_eff ≈ Λ_prompt + β * T_avg
-- where T_avg is the mean delay time of delayed neutrons
effectiveGenerationTime :: ChainParams -> Seconds
effectiveGenerationTime params =
  generationTime params + totalBeta * meanDelayTime
  where
    meanDelayTime = sum (map weightedDelay u235DelayedGroups) / totalBeta
    weightedDelay g = betaFraction g * (log 2 / decayConst g)

-- | Criticality classification
data Criticality
  = Subcritical    -- ^ k < 1: population decays
  | Critical       -- ^ k = 1: population constant
  | Supercritical  -- ^ k > 1: population grows
  | PromptCritical -- ^ k > 1 + β: prompt supercritical (dangerous)
  deriving (Show, Eq, Ord)

-- | Classify criticality of reactor parameters
criticalityOf :: ChainParams -> Criticality
criticalityOf params
  | k < 1.0           = Subcritical
  | k == 1.0          = Critical
  | k < 1.0 + totalBeta = Supercritical
  | otherwise         = PromptCritical
  where k = kEffective params

-- | Reactor period (e-folding time)
--
-- T = Λ_eff / (k - 1)
-- Positive for supercritical, negative for subcritical
reactorPeriod :: ChainParams -> Maybe Seconds
reactorPeriod params
  | k == 1.0  = Nothing  -- Infinite period at critical
  | otherwise = Just $ effectiveGenerationTime params / (k - 1)
  where k = kEffective params
