{-|
Module      : FunctorialFission.Core
Description : Core data types for nuclear states and configurations
Copyright   : (c) Matthew Long, 2025
License     : MIT
Maintainer  : matthew@magneton.ai
Stability   : experimental

This module defines the fundamental data types for representing nuclear
states in the functorial fission framework. Nuclear configurations form
the objects of the category Nucl.
-}

module FunctorialFission.Core
  ( -- * Core Types
    Nuclide(..)
  , NuclearConfig(..)
  , MeV
  , Seconds
    -- * Smart Constructors
  , mkNuclide
  , neutron
  , proton
  , uranium235
  , uranium236
  , uranium238
  , plutonium239
  , barium141
  , krypton92
  , iron56
  , helium4
    -- * Nuclide Properties
  , neutronNumber
  , isStable
  , isFissile
    -- * Configuration Operations
  , emptyConfig
  , singleton
  , combineConfigs
  , configFromList
  , configToList
    -- * Conservation Quantities
  , totalCharge
  , totalBaryon
  , totalNuclides
  ) where

import Data.List (sort)

-- | Energy in MeV
type MeV = Double

-- | Time in seconds
type Seconds = Double

-- | A nuclide is specified by mass number A and atomic number Z.
--
-- In the categorical framework, nuclides are the "atoms" from which
-- nuclear configurations (objects in Nucl) are built.
data Nuclide = Nuclide
  { massNumber   :: !Int  -- ^ A (total nucleons = protons + neutrons)
  , atomicNumber :: !Int  -- ^ Z (protons)
  } deriving (Eq, Ord)

instance Show Nuclide where
  show n = elementSymbol (atomicNumber n) ++ "-" ++ show (massNumber n)

-- | Element symbols for display
elementSymbol :: Int -> String
elementSymbol z = case z of
  0  -> "n"   -- neutron
  1  -> "H"
  2  -> "He"
  6  -> "C"
  8  -> "O"
  26 -> "Fe"
  36 -> "Kr"
  56 -> "Ba"
  92 -> "U"
  94 -> "Pu"
  _  -> "X" ++ show z

-- | Smart constructor for nuclides with validation
mkNuclide :: Int -> Int -> Maybe Nuclide
mkNuclide a z
  | a <= 0    = Nothing
  | z < 0     = Nothing
  | z > a     = Nothing  -- Can't have more protons than nucleons
  | otherwise = Just (Nuclide a z)

-- | Neutron number N = A - Z
neutronNumber :: Nuclide -> Int
neutronNumber n = massNumber n - atomicNumber n

-- | Simple stability check (very approximate)
isStable :: Nuclide -> Bool
isStable n = abs (neutronNumber n - atomicNumber n) <= maxAsymmetry
  where
    a = massNumber n
    -- Allow more neutron excess for heavier nuclei
    maxAsymmetry = max 1 (a `div` 10)

-- | Check if a nuclide is fissile (can sustain chain reaction with thermal neutrons)
isFissile :: Nuclide -> Bool
isFissile n = (massNumber n, atomicNumber n) `elem` fissileNuclides
  where
    fissileNuclides = [(233, 92), (235, 92), (239, 94), (241, 94)]

-- * Common Nuclides

-- | Free neutron
neutron :: Nuclide
neutron = Nuclide 1 0

-- | Proton (hydrogen-1)
proton :: Nuclide
proton = Nuclide 1 1

-- | Uranium-235 (fissile)
uranium235 :: Nuclide
uranium235 = Nuclide 235 92

-- | Uranium-236 (compound nucleus)
uranium236 :: Nuclide
uranium236 = Nuclide 236 92

-- | Uranium-238
uranium238 :: Nuclide
uranium238 = Nuclide 238 92

-- | Plutonium-239 (fissile)
plutonium239 :: Nuclide
plutonium239 = Nuclide 239 94

-- | Barium-141 (fission product)
barium141 :: Nuclide
barium141 = Nuclide 141 56

-- | Krypton-92 (fission product)
krypton92 :: Nuclide
krypton92 = Nuclide 92 36

-- | Iron-56 (maximum binding energy per nucleon)
iron56 :: Nuclide
iron56 = Nuclide 56 26

-- | Helium-4 (alpha particle)
helium4 :: Nuclide
helium4 = Nuclide 4 2

-- | A nuclear configuration is a multiset of nuclides.
--
-- In the categorical framework, NuclearConfig values are the objects
-- of the category Nucl. The monoidal structure is given by combining
-- configurations.
newtype NuclearConfig = NuclearConfig
  { nuclides :: [Nuclide]
  } deriving (Eq)

instance Show NuclearConfig where
  show (NuclearConfig ns) =
    if null ns
    then "∅"
    else unwords $ map showWithCount $ group' $ sort ns
    where
      showWithCount (n, 1) = show n
      showWithCount (n, c) = show c ++ "×" ++ show n

      group' [] = []
      group' (x:xs) = let (same, rest) = span (== x) xs
                      in (x, 1 + length same) : group' rest

-- | The monoidal unit (vacuum/empty configuration)
emptyConfig :: NuclearConfig
emptyConfig = NuclearConfig []

-- | Create a configuration with a single nuclide
singleton :: Nuclide -> NuclearConfig
singleton n = NuclearConfig [n]

-- | Monoidal product: combining two configurations
--
-- This is the tensor product in the symmetric monoidal category Nucl.
combineConfigs :: NuclearConfig -> NuclearConfig -> NuclearConfig
combineConfigs (NuclearConfig xs) (NuclearConfig ys) =
  NuclearConfig (xs ++ ys)

-- | Create a configuration from a list of nuclides
configFromList :: [Nuclide] -> NuclearConfig
configFromList = NuclearConfig

-- | Convert a configuration to a list of nuclides
configToList :: NuclearConfig -> [Nuclide]
configToList = nuclides

-- | Total charge (sum of atomic numbers)
--
-- This is a conserved quantity: any valid morphism in Nucl must preserve total charge.
totalCharge :: NuclearConfig -> Int
totalCharge (NuclearConfig ns) = sum $ map atomicNumber ns

-- | Total baryon number (sum of mass numbers)
--
-- This is a conserved quantity: any valid morphism in Nucl must preserve total baryon number.
totalBaryon :: NuclearConfig -> Int
totalBaryon (NuclearConfig ns) = sum $ map massNumber ns

-- | Count of nuclides in a configuration
totalNuclides :: NuclearConfig -> Int
totalNuclides (NuclearConfig ns) = length ns

-- | Semigroup instance for configurations (monoidal product)
instance Semigroup NuclearConfig where
  (<>) = combineConfigs

-- | Monoid instance for configurations
instance Monoid NuclearConfig where
  mempty = emptyConfig
