module Test.Variant where

import Prelude

import Data.Variant (Variant, inj)
import Data.Variant.Prefix as Prefix
import Effect (Effect)
import Test.Assert (assert)
import Type.Prelude (Proxy(..))

foo :: Variant (bar :: Int, baz :: Boolean)
foo = inj (Proxy :: Proxy "bar") 1

prefoo :: Variant (prebar :: Int, prebaz :: Boolean)
prefoo = inj (Proxy :: Proxy "prebar") 1

pr :: Proxy "pr"
pr = Proxy

e :: Proxy "e"
e = Proxy

suite :: Effect Unit
suite = do
  assert $ Prefix.add pr (Prefix.add e foo) == prefoo
  assert $ Prefix.remove e (Prefix.remove pr prefoo) == foo
  assert $ foo == (Prefix.remove pr $ Prefix.add pr foo)
