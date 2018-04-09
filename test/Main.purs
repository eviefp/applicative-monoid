module Test.Main where

import Prelude

import Data.Maybe (Maybe)
import MA (TMApplicative, TMMonoid)
import Test.MA (checkMA)
import Test.QuickCheck (QC)
import Test.QuickCheck.Laws.Control (checkApplicative)
import Type.Proxy (Proxy2(Proxy2))


main :: QC () Unit
main = do
  checkMA (Proxy2 ∷ Proxy2 (TMApplicative Maybe))
  checkApplicative (Proxy2 ∷ Proxy2 (TMMonoid Maybe))
