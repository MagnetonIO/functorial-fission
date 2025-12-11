{-|
Module      : FunctorialFission
Description : A Categorical Framework for Nuclear Fission Physics
Copyright   : (c) Matthew Long, 2025
License     : MIT
Maintainer  : matthew@magneton.ai
Stability   : experimental

= Functorial Fission

This library provides a categorical reformulation of nuclear fission physics,
treating nuclear states as objects in a symmetric monoidal category and fission
processes as morphisms preserving essential structure.

== Key Concepts

* __Category Nucl__: Nuclear configurations form objects; physical processes are morphisms
* __Binding Energy Functor__: @B: Nucl → Energy@ maps configurations to their binding energies
* __Conservation Laws__: Charge and baryon conservation constrain which morphisms exist
* __Coalgebraic Dynamics__: Chain reactions modeled as coalgebras over branching functors

== Module Structure

* "FunctorialFission.Core" - Basic data types for nuclides and configurations
* "FunctorialFission.SEMF" - Semi-Empirical Mass Formula for binding energies
* "FunctorialFission.Functor" - The binding energy functor and morphisms
* "FunctorialFission.Coalgebra" - Coalgebraic chain reaction dynamics
* "FunctorialFission.Reactor" - Practical reactor calculations
* "FunctorialFission.TypeSafe" - Type-level conservation law enforcement

== Quick Example

@
import FunctorialFission

-- Define the fission process: U-236 → Ba-141 + Kr-92 + 3n
source = singleton uranium236
target = configFromList [barium141, krypton92, neutron, neutron, neutron]
fission = NuclearMorphism source target "U-236 fission"

-- Calculate energy release using the binding energy functor
energy = energyRelease standardCoefficients fission
-- energy ≈ 173 MeV
@

== References

* Semi-empirical mass formula: Weizsäcker (1935), Bethe & Bacher (1936)
* Categorical framework: Mac Lane, "Categories for the Working Mathematician"
* Coalgebraic semantics: Rutten, "Universal Coalgebra: A Theory of Systems"
-}

module FunctorialFission
  ( -- * Core Types
    module FunctorialFission.Core
    -- * Binding Energy (SEMF)
  , module FunctorialFission.SEMF
    -- * Categorical Framework
  , module FunctorialFission.Functor
    -- * Chain Reaction Dynamics
  , module FunctorialFission.Coalgebra
    -- * Reactor Calculations
  , module FunctorialFission.Reactor
  ) where

import FunctorialFission.Core
import FunctorialFission.SEMF
import FunctorialFission.Functor
import FunctorialFission.Coalgebra
import FunctorialFission.Reactor
