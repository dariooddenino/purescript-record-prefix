module Test.Record where

import Prelude
import Effect (Effect)
import Test.Assert (assert)
import Record.Prefix as Prefix
import Type.Prelude (Proxy(..))

foo :: { bar :: Int, baz :: Boolean }
foo = { bar: 1, baz: true }

prefoo :: { prebar :: Int, prebaz :: Boolean }
prefoo = { prebar: 1, prebaz: true }

pr :: Proxy "pr"
pr = Proxy

e :: Proxy "e"
e = Proxy

suite :: Effect Unit
suite = do
  assert $ Prefix.add pr (Prefix.add e foo) == prefoo
  assert $ Prefix.remove e (Prefix.remove pr prefoo) == foo
  assert $ foo == (Prefix.remove pr $ Prefix.add pr foo)
