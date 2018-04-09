module MA where

import Control.Apply (class Apply, (<#>), (<$))
import Data.Eq (class Eq)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(Tuple))
import Prelude (class Applicative, class Functor, Unit, pure, unit, ($), (<$>), (<*>), (<<<))
import Test.QuickCheck (class Arbitrary)


class TupleMonoid f where
  tappend ∷ ∀ a b. f a → f b → f (Tuple a b)
  tempty  ∷ f Unit

instance maybeTupleMonoid ∷ TupleMonoid Maybe where
  tappend fa fb = Tuple <$> fa <*> fb
  tempty = Just unit

newtype TMApplicative f a = TMApplicative (f a)

derive newtype instance eqTMApp ∷ Eq (f a) ⇒ Eq (TMApplicative f a)
derive newtype instance funTMApp ∷ Functor f ⇒ Functor (TMApplicative f)
derive newtype instance applyTMApp ∷ Apply f ⇒ Apply (TMApplicative f)
derive newtype instance appTMApp ∷ Applicative f ⇒ Applicative (TMApplicative f)
derive newtype instance arbTMApp ∷ Arbitrary (f a) ⇒ Arbitrary (TMApplicative f a)

instance tmMonoid ∷ Applicative f ⇒ TupleMonoid (TMApplicative f) where
  tappend fa fb = Tuple <$> fa <*> fb
  tempty = TMApplicative <<< pure $ unit

infixr 5 tappend as <+>

newtype TMMonoid f a = TMMonoid (f a)


derive newtype instance eqTMMon  ∷ Eq (f a) ⇒ Eq (TMMonoid f a)
derive newtype instance funTMMon ∷ Functor f ⇒ Functor (TMMonoid f)
derive newtype instance tmTMMon  ∷ TupleMonoid f ⇒ TupleMonoid (TMMonoid f)
derive newtype instance arbTMMon ∷ Arbitrary (f a) ⇒ Arbitrary (TMMonoid f a)

instance applyTMMon ∷ (Functor f, TupleMonoid f) ⇒ Apply (TMMonoid f) where
  apply fab fa = tappend fab fa <#> \(Tuple ab a) → ab a

instance appTMMon ∷ (Functor f, TupleMonoid f) ⇒ Applicative (TMMonoid f) where
  pure a = a <$ tempty
