module Test.Main where

import Prelude
import Effect (Effect)
import Test.Assert (assert)
import Record.Prefix (addPrefix)
import Type.Prelude (SProxy(..))

foo = { bar: 1, baz: true }

main :: Effect Unit
main = do
  assert $ addPrefix (SProxy :: SProxy "pre") foo == { prebar: 1, prebaz: true }
