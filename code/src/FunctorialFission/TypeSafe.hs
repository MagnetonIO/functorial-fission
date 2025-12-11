{-|
Module      : FunctorialFission.TypeSafe
Description : Type-level encoding of conservation laws
Copyright   : (c) Matthew Long, 2025
License     : MIT
Maintainer  : matthew@magneton.ai
Stability   : experimental

This module demonstrates how conservation laws can be enforced at compile time
using Haskell's type system with GADTs, DataKinds, and type families.

Key ideas:
* Nuclide mass and charge numbers are lifted to the type level
* Nuclear configurations are type-level lists
* Conservation is expressed as type equality constraints
* Invalid fission morphisms fail to compile

This exemplifies the "propositions as types" paradigm where physical laws
become type-level constraints verified by the compiler.

WARNING: This module requires GHC extensions and is primarily pedagogical.
For production use, the runtime-checked version in FunctorialFission.Functor
is more practical.
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module FunctorialFission.TypeSafe
  ( -- * Type-Level Nuclides
    TNuclide(..)
    -- * Type-Level Configurations
  , TConfig(..)
  , TEmpty
  , TCons
    -- * Type Families for Conservation
  , TotalZ
  , TotalA
    -- * Safe Fission Morphisms
  , SafeFission(..)
    -- * Examples
  , exampleFission
    -- * Reflection
  , ConfigReify(..)
  ) where

import GHC.TypeLits
import Data.Kind (Type)
import Data.Proxy

-- ============================================================================
-- Type-Level Nuclides
-- ============================================================================

-- | Type-level representation of a nuclide.
--
-- A nuclide at the type level is parameterized by:
--   * @a@: mass number A (type-level natural)
--   * @z@: atomic number Z (type-level natural)
--
-- The data constructor carries no runtime information; all data is in the type.
data TNuclide (a :: Nat) (z :: Nat) = TNuclide
  deriving (Show)

-- ============================================================================
-- Type-Level Configurations
-- ============================================================================

-- | Type-level nuclear configuration as a type-level list of nuclides.
--
-- Using a GADT ensures we can pattern match on the structure.
type TConfig :: [Type] -> Type
data TConfig cfg where
  TEmpty :: TConfig '[]
  TCons  :: TNuclide a z -> TConfig rest -> TConfig (TNuclide a z ': rest)

instance Show (TConfig '[]) where
  show TEmpty = "∅"

instance (KnownNat a, KnownNat z, Show (TConfig rest))
         => Show (TConfig (TNuclide a z ': rest)) where
  show (TCons _ rest) =
    let a = natVal (Proxy :: Proxy a)
        z = natVal (Proxy :: Proxy z)
    in show a ++ "/" ++ show z ++ " ⊕ " ++ show rest

-- ============================================================================
-- Type Families for Conservation Laws
-- ============================================================================

-- | Type family computing total charge (sum of atomic numbers Z)
type family TotalZ (config :: [Type]) :: Nat where
  TotalZ '[] = 0
  TotalZ (TNuclide a z ': rest) = z + TotalZ rest

-- | Type family computing total baryon number (sum of mass numbers A)
type family TotalA (config :: [Type]) :: Nat where
  TotalA '[] = 0
  TotalA (TNuclide a z ': rest) = a + TotalA rest

-- ============================================================================
-- Type-Safe Fission Morphisms
-- ============================================================================

-- | A type-safe fission morphism that enforces conservation at compile time.
--
-- The GADT constraints @TotalZ src ~ TotalZ tgt@ and @TotalA src ~ TotalA tgt@
-- ensure that charge and baryon number are conserved. If you try to construct
-- a @SafeFission@ that violates conservation, the code won't compile!
--
-- Example: This compiles because U-236 → Ba-141 + Kr-92 + 3n conserves both:
--   * Charge: 92 = 56 + 36 + 0 + 0 + 0 ✓
--   * Baryon: 236 = 141 + 92 + 1 + 1 + 1 ✓
--
-- But @SafeFission@ for U-236 → Ba-141 + Kr-92 would NOT compile:
--   * Baryon: 236 ≠ 141 + 92 = 233 ✗
data SafeFission src tgt where
  SafeFission :: (TotalZ src ~ TotalZ tgt, TotalA src ~ TotalA tgt)
              => TConfig src
              -> TConfig tgt
              -> SafeFission src tgt

instance (Show (TConfig src), Show (TConfig tgt))
         => Show (SafeFission src tgt) where
  show (SafeFission src tgt) = show src ++ " → " ++ show tgt

-- ============================================================================
-- Examples
-- ============================================================================

-- | Example: Thermal neutron-induced fission of U-235
--
-- U-235 + n → U-236* → Ba-141 + Kr-92 + 3n
--
-- Type-level verification:
--   * Source: U-236 (compound nucleus after neutron capture)
--     A = 236, Z = 92
--   * Target: Ba-141 + Kr-92 + n + n + n
--     A = 141 + 92 + 1 + 1 + 1 = 236 ✓
--     Z = 56 + 36 + 0 + 0 + 0 = 92 ✓
--
-- This compiles because the type families compute equal values.
-- Try changing one number and watch it fail!
exampleFission :: SafeFission
  '[TNuclide 236 92]  -- U-236 (compound nucleus)
  '[TNuclide 141 56, TNuclide 92 36, TNuclide 1 0, TNuclide 1 0, TNuclide 1 0]
exampleFission = SafeFission
  (TCons TNuclide TEmpty)  -- U-236
  (TCons TNuclide          -- Ba-141
    (TCons TNuclide        -- Kr-92
      (TCons TNuclide      -- n
        (TCons TNuclide    -- n
          (TCons TNuclide  -- n
            TEmpty)))))

-- ============================================================================
-- Reflection (Type to Value)
-- ============================================================================

-- | Class for reflecting type-level configurations to values
class ConfigReify (cfg :: [Type]) where
  reifyConfig :: TConfig cfg -> [(Int, Int)]

instance ConfigReify '[] where
  reifyConfig TEmpty = []

instance (KnownNat a, KnownNat z, ConfigReify rest)
         => ConfigReify (TNuclide a z ': rest) where
  reifyConfig (TCons _ rest) =
    let a = fromIntegral $ natVal (Proxy :: Proxy a)
        z = fromIntegral $ natVal (Proxy :: Proxy z)
    in (a, z) : reifyConfig rest

-- ============================================================================
-- Additional Type-Level Utilities (for extension)
-- ============================================================================

-- Note: These type families could be extended to compute:
--   * Binding energy differences at the type level
--   * Neutron yield
--   * Q-value bounds
--
-- However, type-level arithmetic in Haskell is limited, so for serious
-- calculations the runtime version is more practical.
