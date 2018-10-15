module Record.Prefix (PrefixProps, addPrefix, UnPrefixProps, removePrefix) where

import Prelude

import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Prim.Row as Row
import Prim.Symbol (class Append)
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Prelude (class IsSymbol, SProxy(..))

data PrefixProps sym = PrefixProps (SProxy sym)

instance prefixProps ::
  ( IsSymbol newsym
  , Append presym sym newsym
  , Row.Lacks newsym rb
  , Row.Cons newsym a rb rc
  ) =>
  FoldingWithIndex
    (PrefixProps presym)
    (SProxy sym)
    (Builder { | ra } { | rb })
    a
    (Builder { | ra } { | rc })
  where
  foldingWithIndex (PrefixProps _) _ rin a =
    (_ >>> Builder.insert (SProxy :: SProxy newsym) a) rin

-- | Adds a common prefix to a Record's labels.
addPrefix ::
  forall rin rout pre.
  HFoldlWithIndex (PrefixProps pre) (Builder {} {}) { | rin } (Builder {} { | rout }) =>
  SProxy pre ->
  { | rin } ->
  { | rout }
addPrefix pre = flip Builder.build {} <<< hfoldlWithIndex (PrefixProps pre) identity

data UnPrefixProps sym = UnPrefixProps (SProxy sym)

instance unprefixProps ::
  ( IsSymbol newsym
  , Append presym newsym sym
  , Row.Lacks newsym rb
  , Row.Cons newsym a rb rc
  ) =>
  FoldingWithIndex
    (UnPrefixProps presym)
    (SProxy sym)
    (Builder { | ra } { | rb })
    a
    (Builder { | ra } { | rc })
  where
  foldingWithIndex (UnPrefixProps _) _ rin a =
    (_ >>> Builder.insert (SProxy :: SProxy newsym) a) rin

-- | Removes a common prefix from a Record's labels.
removePrefix ::
  forall rin rout pre.
  HFoldlWithIndex (UnPrefixProps pre) (Builder {} {}) { | rin } (Builder {} { | rout }) =>
  SProxy pre ->
  { | rin } ->
  { | rout }
removePrefix pre = flip Builder.build {} <<< hfoldlWithIndex (UnPrefixProps pre) identity
