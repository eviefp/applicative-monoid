module Test.TypeA where

import Prelude

import Test.QuickCheck (class Arbitrary, class Coarbitrary, arbitrary, coarbitrary)

newtype A = A Ordering

instance eqA ∷ Eq A where
  eq (A x) (A y) = eq x y

instance arbitraryA ∷ Arbitrary A where
  arbitrary = A <$> arbitrary

instance coarbitraryA ∷ Coarbitrary A where
  coarbitrary (A x) = coarbitrary x
