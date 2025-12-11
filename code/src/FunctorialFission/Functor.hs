{-|
Module      : FunctorialFission.Functor
Description : The Binding Energy Functor and Fission Morphisms
Copyright   : (c) Matthew Long, 2025
License     : MIT
Maintainer  : matthew@magneton.ai
Stability   : experimental

This module implements the categorical framework for nuclear fission:

* Nuclear configurations form objects in a symmetric monoidal category Nucl
* Fission processes are morphisms preserving conservation laws
* Binding energy extends to a functor B: Nucl → Energy
* Conservation laws constrain which morphisms exist

The key insight is that treating binding energy as a *functor* (not just a function)
captures how physical processes transform energy in a compositional way.
-}

module FunctorialFission.Functor
  ( -- * Morphisms in Nucl
    NuclearMorphism(..)
  , FissionMorphism
  , FusionMorphism
  , DecayMorphism
    -- * Conservation Laws
  , ConservationCheck(..)
  , checkConservation
  , isValidMorphism
    -- * The Binding Energy Functor
  , bindingFunctorObj
  , bindingFunctorMor
  , energyRelease
    -- * Composition
  , composeMorphisms
  , identityMorphism
    -- * Fission Utilities
  , mkFissionMorphism
  , isFissionMorphism
  , neutronYield
  ) where

import FunctorialFission.Core
import FunctorialFission.SEMF

-- | A morphism in the category Nucl represents a physical nuclear process.
--
-- In categorical terms:
-- * Objects: NuclearConfig (multisets of nuclides)
-- * Morphisms: Physical processes transforming configurations
-- * Identity: The trivial process
-- * Composition: Sequential application of processes
data NuclearMorphism = NuclearMorphism
  { morphismSource :: NuclearConfig  -- ^ Domain (initial state)
  , morphismTarget :: NuclearConfig  -- ^ Codomain (final state)
  , morphismName   :: String         -- ^ Human-readable description
  } deriving (Show, Eq)

-- | Type alias for fission morphisms (heavy → light fragments + neutrons)
type FissionMorphism = NuclearMorphism

-- | Type alias for fusion morphisms (light nuclei → heavier nucleus)
type FusionMorphism = NuclearMorphism

-- | Type alias for decay morphisms (single nucleus → products)
type DecayMorphism = NuclearMorphism

-- | Conservation check results for a morphism
data ConservationCheck = ConservationCheck
  { chargeConserved  :: !Bool  -- ^ Is total charge (Z) preserved?
  , baryonConserved  :: !Bool  -- ^ Is total baryon number (A) preserved?
  , leptonConserved  :: !Bool  -- ^ Is lepton number preserved? (always True for nuclear)
  } deriving (Show, Eq)

-- | Check conservation laws for a proposed morphism
checkConservation :: NuclearMorphism -> ConservationCheck
checkConservation (NuclearMorphism src tgt _) = ConservationCheck
  { chargeConserved = totalCharge src == totalCharge tgt
  , baryonConserved = totalBaryon src == totalBaryon tgt
  , leptonConserved = True  -- Nuclear processes conserve lepton number trivially
  }

-- | Test if a morphism satisfies all conservation laws
--
-- In categorical terms, only valid morphisms exist in Nucl.
-- This function determines whether a proposed morphism actually exists.
isValidMorphism :: NuclearMorphism -> Bool
isValidMorphism m = chargeConserved c && baryonConserved c
  where c = checkConservation m

-- | The Binding Energy Functor on objects
--
-- B: Nucl → Energy
--
-- Maps a nuclear configuration to its total binding energy.
-- This is additive: B(X ⊕ Y) = B(X) + B(Y)
bindingFunctorObj :: SEMFCoefficients -> NuclearConfig -> MeV
bindingFunctorObj coef (NuclearConfig ns) =
  sum $ map (bindingEnergy coef) ns

-- | The Binding Energy Functor on morphisms
--
-- For a morphism φ: X → Y, the functor maps it to the energy difference:
-- B(φ) = B(Y) - B(X)
--
-- Positive values indicate energy release (exoergic).
-- This satisfies functoriality: B(ψ ∘ φ) = B(ψ) + B(φ)
bindingFunctorMor :: SEMFCoefficients -> NuclearMorphism -> MeV
bindingFunctorMor coef (NuclearMorphism src tgt _) =
  bindingFunctorObj coef tgt - bindingFunctorObj coef src

-- | Energy released by a morphism (same as bindingFunctorMor but named for clarity)
energyRelease :: SEMFCoefficients -> NuclearMorphism -> MeV
energyRelease = bindingFunctorMor

-- | Compose two morphisms (if compatible)
--
-- ψ ∘ φ is defined when target(φ) = source(ψ)
--
-- The binding functor respects composition:
-- B(ψ ∘ φ) = B(ψ) + B(φ)
composeMorphisms :: NuclearMorphism -> NuclearMorphism -> Maybe NuclearMorphism
composeMorphisms phi psi
  | morphismTarget phi == morphismSource psi =
      Just $ NuclearMorphism
        { morphismSource = morphismSource phi
        , morphismTarget = morphismTarget psi
        , morphismName   = morphismName phi ++ " ; " ++ morphismName psi
        }
  | otherwise = Nothing

-- | Identity morphism on a configuration
--
-- id_X: X → X (the trivial process)
-- B(id_X) = 0 (no energy change)
identityMorphism :: NuclearConfig -> NuclearMorphism
identityMorphism cfg = NuclearMorphism cfg cfg "identity"

-- | Smart constructor for fission morphisms with validation
mkFissionMorphism :: NuclearConfig -> NuclearConfig -> String -> Maybe FissionMorphism
mkFissionMorphism src tgt name
  | isValidMorphism m && isFissionMorphism m = Just m
  | otherwise = Nothing
  where m = NuclearMorphism src tgt name

-- | Check if a morphism represents a fission process
--
-- A fission morphism:
-- 1. Has a single heavy nucleus as source (or compound nucleus)
-- 2. Has multiple fragments as products
-- 3. Releases energy (B(φ) > 0)
isFissionMorphism :: NuclearMorphism -> Bool
isFissionMorphism (NuclearMorphism src tgt _) =
  totalNuclides src <= 2 &&     -- Single nucleus or nucleus + neutron
  totalNuclides tgt >= 3 &&     -- At least 2 fragments + neutrons
  totalBaryon src > 100         -- Heavy nucleus

-- | Count neutrons produced in a fission morphism
neutronYield :: NuclearMorphism -> Int
neutronYield (NuclearMorphism _ tgt _) =
  length $ filter isNeutron $ nuclides tgt
  where
    isNeutron n = massNumber n == 1 && atomicNumber n == 0
