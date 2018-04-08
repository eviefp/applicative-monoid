module Test.MA where


import Control.Monad.Eff.Console (log)
import Data.Tuple (Tuple(..))
import MA (class MonoidApplicative, (<>), mempty)
import Prelude (class Eq, class Functor, Ordering, Unit, discard, eq, map, ($), (<$>), (<<<))
import Test.QuickCheck (class Arbitrary, class Coarbitrary, QC, arbitrary, coarbitrary, quickCheck')
import Type.Proxy (Proxy2)


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



checkMA ∷ ∀ eff f
        . MonoidApplicative f
        ⇒ Arbitrary (f A)
        ⇒ Functor f
        ⇒ Eq (f (Tuple A (Tuple A A)))
        ⇒ Eq (f A)
        ⇒ Proxy2 f
        → QC eff Unit
checkMA _ = do
  log "Checking 'Associativity' law for MonoidApplicative"
  quickCheck' 1000 associativity
  log "Checking 'Left Identity' law for MonoidApplicative"
  quickCheck' 1000 leftIdentity
  log "Checking 'Right Identity' law for MonoidApplicative"
  quickCheck' 1000 rightIdentity

    where

    isoTuple ∷ ∀ a b c. Tuple (Tuple a b) c → Tuple a (Tuple b c)
    isoTuple (Tuple (Tuple a b) c) = Tuple a (Tuple b c)

    isoLeftUnit ∷ ∀ a. Tuple Unit a → a
    isoLeftUnit (Tuple _ a) = a

    isoRightUnit ∷ ∀ a. Tuple a Unit → a
    isoRightUnit (Tuple a _) = a

    associativity ∷ f A → f A → f A → Boolean
    associativity x y z =
      eq left <<< map isoTuple $ right

      where

      left = x <> (y <> z)
      right = (x <> y) <> z

    leftIdentity ∷ f A → Boolean
    leftIdentity fa =
      eq fa <<< map isoLeftUnit $ mempty <> fa

    rightIdentity ∷ f A → Boolean
    rightIdentity fa =
      eq fa <<< map isoRightUnit $ fa <> mempty