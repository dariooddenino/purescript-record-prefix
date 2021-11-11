module Test.Main where

import Test.Record as Record
import Test.Variant as Variant

import Effect (Effect)
import Prelude

main :: Effect Unit
main = do
  Record.suite
  Variant.suite
