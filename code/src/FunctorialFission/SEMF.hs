{-|
Module      : FunctorialFission.SEMF
Description : Semi-Empirical Mass Formula for nuclear binding energies
Copyright   : (c) Matthew Long, 2025
License     : MIT
Maintainer  : matthew@magneton.ai
Stability   : experimental

This module implements the Semi-Empirical Mass Formula (SEMF), also known
as the Bethe-Weizsäcker formula, for calculating nuclear binding energies.

The SEMF models binding energy as:

@
B(A,Z) = a_v·A - a_s·A^(2/3) - a_c·Z²/A^(1/3) - a_a·(A-2Z)²/A + δ(A,Z)
@

where each term has a physical interpretation:
  * Volume term: bulk binding from nuclear force
  * Surface term: surface nucleons have fewer neighbors
  * Coulomb term: proton-proton electrostatic repulsion
  * Asymmetry term: Pauli exclusion favors N ≈ Z
  * Pairing term: nucleons prefer paired states
-}

module FunctorialFission.SEMF
  ( -- * Coefficients
    SEMFCoefficients(..)
  , standardCoefficients
  , fitCoefficients
    -- * Pairing
  , Pairing(..)
  , pairingType
  , pairingTerm
    -- * Binding Energy Calculations
  , bindingEnergy
  , bindingPerNucleon
  , massDefect
  , nuclearMass
    -- * Individual Terms (for analysis)
  , volumeTerm
  , surfaceTerm
  , coulombTerm
  , asymmetryTerm
    -- * Utilities
  , qValue
  , separationEnergy
  ) where

import FunctorialFission.Core

-- | Coefficients for the Semi-Empirical Mass Formula (all in MeV)
data SEMFCoefficients = SEMFCoefficients
  { aVolume    :: !MeV  -- ^ Volume coefficient (~15.8 MeV)
  , aSurface   :: !MeV  -- ^ Surface coefficient (~18.3 MeV)
  , aCoulomb   :: !MeV  -- ^ Coulomb coefficient (~0.714 MeV)
  , aAsymmetry :: !MeV  -- ^ Asymmetry coefficient (~23.2 MeV)
  , aPairing   :: !MeV  -- ^ Pairing coefficient (~12.0 MeV)
  } deriving (Show, Eq)

-- | Standard SEMF coefficients from fits to nuclear data
standardCoefficients :: SEMFCoefficients
standardCoefficients = SEMFCoefficients
  { aVolume    = 15.8
  , aSurface   = 18.3
  , aCoulomb   = 0.714
  , aAsymmetry = 23.2
  , aPairing   = 12.0
  }

-- | Alternative fit coefficients (Rohlf 1994)
fitCoefficients :: SEMFCoefficients
fitCoefficients = SEMFCoefficients
  { aVolume    = 15.75
  , aSurface   = 17.8
  , aCoulomb   = 0.711
  , aAsymmetry = 23.7
  , aPairing   = 11.2
  }

-- | Pairing term classification based on even/odd nucleon counts
data Pairing
  = EvenEven  -- ^ Even Z, even N: +δ (most stable)
  | OddA      -- ^ Odd A: 0
  | OddOdd    -- ^ Odd Z, odd N: -δ (least stable)
  deriving (Eq, Show, Ord)

-- | Determine the pairing type of a nuclide
pairingType :: Nuclide -> Pairing
pairingType n
  | even z && even (a - z) = EvenEven
  | odd a                  = OddA
  | otherwise              = OddOdd
  where
    a = massNumber n
    z = atomicNumber n

-- | Calculate the pairing term δ(A,Z)
pairingTerm :: SEMFCoefficients -> Nuclide -> MeV
pairingTerm coef n = case pairingType n of
  EvenEven -> aPairing coef / sqrt (fromIntegral a)
  OddA     -> 0
  OddOdd   -> negate (aPairing coef) / sqrt (fromIntegral a)
  where
    a = massNumber n

-- | Volume term: a_v · A
--
-- Reflects saturation of nuclear force - each nucleon interacts only
-- with nearest neighbors, so binding scales with volume ∝ A.
volumeTerm :: SEMFCoefficients -> Nuclide -> MeV
volumeTerm coef n = aVolume coef * fromIntegral (massNumber n)

-- | Surface term: -a_s · A^(2/3)
--
-- Surface nucleons have fewer neighbors, reducing binding.
-- Surface area scales as A^(2/3).
surfaceTerm :: SEMFCoefficients -> Nuclide -> MeV
surfaceTerm coef n = aSurface coef * (fromIntegral (massNumber n) ** (2/3))

-- | Coulomb term: -a_c · Z² / A^(1/3)
--
-- Electrostatic repulsion between protons. For Z protons in a sphere
-- of radius R ∝ A^(1/3), Coulomb energy ∝ Z²/R.
coulombTerm :: SEMFCoefficients -> Nuclide -> MeV
coulombTerm coef n =
  aCoulomb coef * (fromIntegral z * fromIntegral z) / (fromIntegral a ** (1/3))
  where
    a = massNumber n
    z = atomicNumber n

-- | Asymmetry term: -a_a · (A-2Z)² / A
--
-- Pauli exclusion principle favors N ≈ Z. Deviations incur an energy penalty.
asymmetryTerm :: SEMFCoefficients -> Nuclide -> MeV
asymmetryTerm coef n =
  aAsymmetry coef * (fromIntegral (a - 2*z) ^ (2::Int)) / fromIntegral a
  where
    a = massNumber n
    z = atomicNumber n

-- | Calculate total binding energy using the SEMF
--
-- @
-- B(A,Z) = a_v·A - a_s·A^(2/3) - a_c·Z²/A^(1/3) - a_a·(A-2Z)²/A + δ(A,Z)
-- @
bindingEnergy :: SEMFCoefficients -> Nuclide -> MeV
bindingEnergy coef n
  | massNumber n == 1 = 0  -- Free nucleon has zero binding energy
  | otherwise =
      volumeTerm coef n
      - surfaceTerm coef n
      - coulombTerm coef n
      - asymmetryTerm coef n
      + pairingTerm coef n

-- | Binding energy per nucleon: B/A
--
-- This quantity peaks near A ≈ 56 (iron/nickel region) at ~8.8 MeV/nucleon.
-- Heavy nuclei like uranium have B/A ≈ 7.6 MeV/nucleon.
-- This difference is what makes fission energetically favorable.
bindingPerNucleon :: SEMFCoefficients -> Nuclide -> MeV
bindingPerNucleon coef n
  | massNumber n == 0 = 0
  | otherwise = bindingEnergy coef n / fromIntegral (massNumber n)

-- | Mass defect in atomic mass units
--
-- Δm = Z·m_p + N·m_n - M(A,Z)
-- where the binding energy is B = Δm·c²
massDefect :: SEMFCoefficients -> Nuclide -> Double
massDefect coef n = bindingEnergy coef n / amuToMeV
  where
    amuToMeV = 931.5  -- 1 u·c² in MeV

-- | Nuclear mass in atomic mass units
--
-- M(A,Z) = Z·m_p + N·m_n - Δm
nuclearMass :: SEMFCoefficients -> Nuclide -> Double
nuclearMass coef n = z * protonMass + nn * neutronMass - massDefect coef n
  where
    z  = fromIntegral $ atomicNumber n
    nn = fromIntegral $ neutronNumber n
    protonMass  = 1.007276  -- u
    neutronMass = 1.008665  -- u

-- | Q-value of a reaction: energy released
--
-- Q = Σ B(products) - Σ B(reactants)
-- Positive Q means exoergic (energy-releasing) reaction.
qValue :: SEMFCoefficients -> [Nuclide] -> [Nuclide] -> MeV
qValue coef reactants products =
  sum (map (bindingEnergy coef) products) - sum (map (bindingEnergy coef) reactants)

-- | Separation energy: energy to remove one nucleon
--
-- S_n = B(A,Z) - B(A-1,Z)      for neutron
-- S_p = B(A,Z) - B(A-1,Z-1)    for proton
separationEnergy :: SEMFCoefficients -> Nuclide -> (MeV, MeV)
separationEnergy coef n = (sn, sp)
  where
    a = massNumber n
    z = atomicNumber n
    sn = bindingEnergy coef n - bindingEnergy coef (Nuclide (a-1) z)      -- neutron
    sp = bindingEnergy coef n - bindingEnergy coef (Nuclide (a-1) (z-1))  -- proton
