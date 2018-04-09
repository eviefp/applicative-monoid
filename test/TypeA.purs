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


newtype B = B Ordering

instance eqB ∷ Eq B where
  eq (B x) (B y) = eq x y

instance arbitraryB ∷ Arbitrary B where
  arbitrary = B <$> arbitrary

instance coarbitraryB ∷ Coarbitrary B where
  coarbitrary (B x) = coarbitrary x


newtype C = C Ordering

instance eqC ∷ Eq C where
  eq (C x) (C y) = eq x y

instance arbitraryC ∷ Arbitrary C where
  arbitrary = C <$> arbitrary

instance coarbitraryC ∷ Coarbitrary C where
  coarbitrary (C x) = coarbitrary x
