module MA where

import Data.Tuple (Tuple(Tuple))
import Prelude (class Applicative, class Functor, Unit, const, pure, unit, (<$>), (<*>))


class MonoidApplicative f where
  append ∷ ∀ a b. f a → f b → f (Tuple a b)
  mempty ∷ f Unit

instance mapp ∷ Applicative f => MonoidApplicative f where
  append fa fb = Tuple <$> fa <*> fb
  mempty = pure unit

infixr 5 append as <>


class ApplicativeMonoid f where
  apply' ∷ ∀ a b. f (a → b) → f a → f b
  pure'  ∷ ∀ a. a → f a

instance appm ∷ (Functor f, MonoidApplicative f) ⇒ ApplicativeMonoid f where
  apply' fab fa = tapply <$> tuple

    where

    tuple = append fab fa
    tapply (Tuple ab a) = ab a

  pure' a = const a <$> mempty
