{-|
Module      : Main
Description : Demonstration of the Functorial Fission framework
Copyright   : (c) Matthew Long, 2025
License     : MIT

Complete worked example demonstrating:
* Nuclear configuration construction
* SEMF binding energy calculations
* Functorial energy release computation
* Conservation law verification
* Chain reaction dynamics
-}

module Main where

import FunctorialFission
import Text.Printf

main :: IO ()
main = do
  putStrLn "╔══════════════════════════════════════════════════════════════════╗"
  putStrLn "║         FUNCTORIAL FISSION: A Categorical Framework              ║"
  putStrLn "║            for Nuclear Fission Physics                           ║"
  putStrLn "╚══════════════════════════════════════════════════════════════════╝"
  putStrLn ""

  -- Standard SEMF coefficients
  let coef = standardCoefficients

  putStrLn "═══════════════════════════════════════════════════════════════════"
  putStrLn "  1. SEMI-EMPIRICAL MASS FORMULA (SEMF)"
  putStrLn "═══════════════════════════════════════════════════════════════════"
  putStrLn ""
  putStrLn "  B(A,Z) = a_v·A - a_s·A^(2/3) - a_c·Z²/A^(1/3) - a_a·(A-2Z)²/A + δ"
  putStrLn ""
  putStrLn "  Coefficients:"
  printf   "    Volume:    a_v = %.2f MeV\n" (aVolume coef)
  printf   "    Surface:   a_s = %.2f MeV\n" (aSurface coef)
  printf   "    Coulomb:   a_c = %.3f MeV\n" (aCoulomb coef)
  printf   "    Asymmetry: a_a = %.2f MeV\n" (aAsymmetry coef)
  printf   "    Pairing:   δ_0 = %.2f MeV\n" (aPairing coef)
  putStrLn ""

  putStrLn "  Binding Energies:"
  putStrLn "  ┌────────────┬───────────────┬───────────────┐"
  putStrLn "  │  Nuclide   │   B (MeV)     │   B/A (MeV)   │"
  putStrLn "  ├────────────┼───────────────┼───────────────┤"
  printNuclide coef uranium236
  printNuclide coef barium141
  printNuclide coef krypton92
  printNuclide coef iron56
  printNuclide coef helium4
  putStrLn "  └────────────┴───────────────┴───────────────┘"
  putStrLn ""

  putStrLn "═══════════════════════════════════════════════════════════════════"
  putStrLn "  2. THE BINDING ENERGY FUNCTOR"
  putStrLn "═══════════════════════════════════════════════════════════════════"
  putStrLn ""
  putStrLn "  Category Nucl:"
  putStrLn "    • Objects: Multisets of nuclides (NuclearConfig)"
  putStrLn "    • Morphisms: Physical processes preserving conservation laws"
  putStrLn ""
  putStrLn "  Functor B: Nucl → Energy"
  putStrLn "    • On objects:  B(X) = Σ B(n_i) for n_i ∈ X"
  putStrLn "    • On morphisms: B(φ) = B(target) - B(source)"
  putStrLn ""

  -- Define configurations
  let source = singleton uranium236
      target = configFromList [barium141, krypton92, neutron, neutron, neutron]
      fissionMorphism = NuclearMorphism source target "U-236 fission"

  putStrLn "  Example: U-236 → Ba-141 + Kr-92 + 3n"
  putStrLn ""
  printf   "    Source: %s\n" (show source)
  printf   "    Target: %s\n" (show target)
  putStrLn ""
  printf   "    B(source) = %.2f MeV\n" (bindingFunctorObj coef source)
  printf   "    B(target) = %.2f MeV\n" (bindingFunctorObj coef target)
  printf   "    B(φ) = Energy Release = %.2f MeV\n" (energyRelease coef fissionMorphism)
  putStrLn ""

  putStrLn "═══════════════════════════════════════════════════════════════════"
  putStrLn "  3. CONSERVATION LAWS"
  putStrLn "═══════════════════════════════════════════════════════════════════"
  putStrLn ""
  putStrLn "  Conservation laws constrain morphism existence in Nucl."
  putStrLn ""

  let check = checkConservation fissionMorphism
  printf   "    Source: A = %d, Z = %d\n" (totalBaryon source) (totalCharge source)
  printf   "    Target: A = %d, Z = %d\n" (totalBaryon target) (totalCharge target)
  putStrLn ""
  printf   "    Charge conserved:  %s\n" (show $ chargeConserved check)
  printf   "    Baryon conserved:  %s\n" (show $ baryonConserved check)
  printf   "    Valid morphism:    %s\n" (show $ isValidMorphism fissionMorphism)
  putStrLn ""

  putStrLn "═══════════════════════════════════════════════════════════════════"
  putStrLn "  4. COALGEBRAIC CHAIN REACTIONS"
  putStrLn "═══════════════════════════════════════════════════════════════════"
  putStrLn ""
  putStrLn "  The branching functor F(X) = (P_fin(X), R+) models neutron multiplication."
  putStrLn "  Chain reactions are coalgebras over F."
  putStrLn ""

  let params = defaultParams { kEffective = 1.0 }
  printf   "  Parameters (critical reactor):\n"
  printf   "    k_eff = %.4f\n" (kEffective params)
  printf   "    Generation time Λ = %.2e s\n" (generationTime params)
  printf   "    Effective Λ (with delayed n) = %.4f s\n" (effectiveGenerationTime params)
  printf   "    Criticality: %s\n" (show $ criticalityOf params)
  putStrLn ""

  let initialN = mkNeutronState 1e10  -- 10 billion neutrons
  printf   "  Initial neutron population: %.2e\n" (population initialN)
  printf   "  After 10 generations: %.2e\n" (populationAfter 10 initialN params)
  printf   "  Energy released (10 gen): %.2f MeV\n" (totalEnergyAfter 10 initialN params)
  putStrLn ""

  -- Supercritical case
  let superParams = params { kEffective = 1.001 }
  case reactorPeriod superParams of
    Just t -> printf "  Supercritical (k=1.001) period: %.2f s\n" t
    Nothing -> putStrLn "  Supercritical period: undefined"
  putStrLn ""

  putStrLn "═══════════════════════════════════════════════════════════════════"
  putStrLn "  5. REACTOR PHYSICS"
  putStrLn "═══════════════════════════════════════════════════════════════════"
  putStrLn ""

  let reactor = defaultReactor
  printf   "  Thermal Power:  %.0f MW\n" (thermalPowerMW reactor)
  printf   "  Electric Power: %.0f MW\n" (electricPowerMW reactor)
  printf   "  Efficiency:     %.1f%%\n" (100 * thermalEfficiency reactor)
  putStrLn ""

  let powerW = thermalPowerMW reactor * 1e6
  printf   "  Fission rate:   %.2e fissions/s\n" (fissionRate powerW)
  printf   "  Fuel consumption: %.4f kg/day\n" (fuelConsumptionRate powerW * 86400)
  putStrLn ""

  putStrLn "═══════════════════════════════════════════════════════════════════"
  putStrLn "  6. SUMMARY"
  putStrLn "═══════════════════════════════════════════════════════════════════"
  putStrLn ""
  putStrLn "  This framework demonstrates:"
  putStrLn "    ✓ Nuclear configurations as categorical objects"
  putStrLn "    ✓ Fission processes as morphisms in Nucl"
  putStrLn "    ✓ Binding energy as a functor B: Nucl → Energy"
  putStrLn "    ✓ Conservation laws as structural constraints"
  putStrLn "    ✓ Chain reactions as coalgebras"
  putStrLn ""
  putStrLn "  The categorical perspective reveals compositional structure"
  putStrLn "  and enables type-safe implementations of nuclear physics."
  putStrLn ""
  putStrLn "══════════════════════════════════════════════════════════════════"

-- Helper to print nuclide data
printNuclide :: SEMFCoefficients -> Nuclide -> IO ()
printNuclide coef n =
  printf "  │ %10s │ %13.2f │ %13.3f │\n"
    (show n)
    (bindingEnergy coef n)
    (bindingPerNucleon coef n)
