module Data.Variant.Prefix where

import Prelude
import Data.Variant (Variant, inj)
import Data.Variant (inj) as Variant
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Prim.Row (class Cons) as Row
import Prim.Symbol (class Append) as Symbol
import Type.Prelude (class IsSymbol, SProxy(..))

data PrefixCases (s :: Symbol)
  = PrefixCases

instance prefixCases ::
  ( Symbol.Append s l l'
  , IsSymbol l'
  , Row.Cons l' a v v'
  ) =>
  FoldingWithIndex (PrefixCases s) (SProxy l) Unit a (Variant v') where
  foldingWithIndex _ prop _ a = Variant.inj (SProxy :: SProxy l') a

data UnprefixCases (s :: Symbol)
  = UnprefixCases

instance unprefixCases ::
  ( Symbol.Append s l' l
  , IsSymbol l'
  , Row.Cons l' a v v'
  ) =>
  FoldingWithIndex (UnprefixCases s) (SProxy l) Unit a (Variant v') where
  foldingWithIndex _ prop _ a = Variant.inj (SProxy :: SProxy l') a

add ::
  forall rin rout pre.
  IsSymbol pre =>
  HFoldlWithIndex (PrefixCases pre) Unit (Variant rin) (Variant rout) =>
  SProxy pre ->
  Variant rin ->
  Variant rout
add p = hfoldlWithIndex (PrefixCases :: PrefixCases pre) unit

remove ::
  forall pre rin rout.
  HFoldlWithIndex (UnprefixCases pre) Unit (Variant rin) (Variant rout) =>
  SProxy pre ->
  Variant rin ->
  Variant rout
remove p = hfoldlWithIndex (UnprefixCases :: UnprefixCases pre) unit

test ::
  forall t27.
  Variant
    ( bar :: Int
    | t27
    )
test = remove (SProxy ∷ SProxy "foo") (inj (SProxy ∷ SProxy "foobar") 8 ∷ Variant ( foobar ∷ Int ))

