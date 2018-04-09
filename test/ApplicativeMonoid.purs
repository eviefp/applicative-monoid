module Test.AM where


import Control.Monad.Eff.Console (log)
import MA (class ApplicativeMonoid, apply', pure')
import Prelude (class Eq, class Functor, Unit, discard, id, ($), (<$>), (<<<), (==))
import Test.QuickCheck (class Arbitrary, QC, quickCheck')
import Test.TypeA (A, B, C)
import Type.Proxy (Proxy2)


checkAM ∷ ∀ eff f
        . ApplicativeMonoid f
        ⇒ Functor f
        ⇒ Arbitrary (f A)
        ⇒ Arbitrary (f (A → B))
        ⇒ Arbitrary (f (B → C))
        ⇒ Eq (f A)
        ⇒ Eq (f B)
        ⇒ Eq (f C)
        ⇒ Proxy2 f
        → QC eff Unit
checkAM _ = do
  log "Checking 'Identity' law for ApplicativeMonoid"
  quickCheck' 1000 identity

  log "Checking 'Associativity' law for ApplicativeMonoid"
  quickCheck' 1000 associativity

  log "Checking 'Homorphism' law for ApplicativeMonoid"
  quickCheck' 1000 homomorphism

  log "Checking 'Interchange' law for ApplicativeMonoid"
  quickCheck' 1000 interchange

    where

    identity ∷ f A → Boolean
    identity v = (pure' id `apply'` v) == v

    associativity ∷ f (B → C) → f (A → B) → f A → Boolean
    associativity f g x =
      (((<<<) <$> f) `apply'` g `apply'` x) == (apply' f (apply' g x))

    homomorphism ∷ (A → B) → A → Boolean
    homomorphism f x = apply' (pure' f) (pure' x)  == (pure' (f x) ∷ f B)

    interchange ∷ A → f (A → B) → Boolean
    interchange y u = apply' u (pure' y) == apply' (pure' (_ $ y)) u
