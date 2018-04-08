module MA where

import Data.Tuple (Tuple(Tuple))
import Prelude (class Applicative, Unit, pure, unit, (<$>), (<*>))


class MonoidApplicative f where
  append ∷ ∀ a b. f a → f b → f (Tuple a b)
  mempty ∷ f Unit

instance mapp ∷ Applicative f => MonoidApplicative f where
  append fa fb = Tuple <$> fa <*> fb
  mempty = pure unit

infixr 5 append as <>
