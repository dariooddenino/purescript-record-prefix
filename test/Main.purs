module Test.Main where

import Prelude
import Effect (Effect)
import Test.Assert (assert)
import Record.Prefix as Prefix
import Type.Prelude (SProxy(..))

foo :: { bar :: Int, baz :: Boolean }
foo = { bar: 1, baz: true }

prefoo :: { prebar :: Int, prebaz :: Boolean }
prefoo = { prebar: 1, prebaz: true }

pre :: SProxy "pre"
pre = SProxy

main :: Effect Unit
main = do
  assert $ Prefix.add pre foo == prefoo
  assert $ Prefix.remove pre prefoo == foo
  assert $ foo == (Prefix.remove pre $ Prefix.add pre foo)
