module Test.Main where

import Prelude

import Data.Maybe (Maybe)
import Test.MA (checkMA)
import Test.QuickCheck (QC)
import Type.Proxy (Proxy2(Proxy2))


main :: QC () Unit
main = do
  checkMA (Proxy2 :: Proxy2 Maybe)