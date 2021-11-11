module Data.Variant.Prefix where

import Prelude
import Data.Variant (Variant, inj)
import Data.Variant (inj) as Variant
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Prim.Row (class Cons) as Row
import Prim.RowList (Cons, Nil) as RowList
import Prim.RowList (RowList)
import Prim.Symbol (class Append) as Symbol
import Type.Eval (class Eval, Lift, TypeExpr)
import Type.Eval.Foldable (FoldrWithIndex)
import Type.Eval.Function (type (<<<))
import Type.Eval.RowList (FromRow, ToRow)
import Type.Prelude (class IsSymbol, Proxy(..))

-- | Without type computations using `PrefixStep`
-- | and `UnprefixStep` composition of functions like
-- | `add` or `remove` requires type annotations
-- | of the intermediate results.
foreign import data PrefixStep :: Symbol -> Symbol -> Type -> TypeExpr (RowList Type) -> TypeExpr (RowList Type)

instance evalPerfixStep ::
  ( Symbol.Append prefix label label'
  , Eval tail tail'
  , IsSymbol label'
  ) =>
  Eval (PrefixStep prefix label value tail) (RowList.Cons label' value tail')

data PrefixCases (s :: Symbol) (result :: Row Type)
  = PrefixCases

instance prefixCases ::
  ( Symbol.Append s l l'
  , IsSymbol l'
  , Row.Cons l' a result_ result
  ) =>
  FoldingWithIndex (PrefixCases s result) (Proxy l) Unit a (Variant result) where
  foldingWithIndex _ _ _ a = Variant.inj (Proxy :: Proxy l') a

foreign import data UnprefixStep :: Symbol -> Symbol -> Type -> TypeExpr (RowList Type) -> TypeExpr (RowList Type)

instance evalUnprefixStep ::
  ( Symbol.Append prefix label' label
  , Eval tail tail'
  , IsSymbol label'
  ) =>
  Eval (UnprefixStep prefix label value tail) (RowList.Cons label' value tail')

data UnprefixCases (s :: Symbol) (result :: Row Type)
  = UnprefixCases

instance unprefixCases ::
  ( Symbol.Append s l' l
  , IsSymbol l'
  , Row.Cons l' a result_ result
  ) =>
  FoldingWithIndex (UnprefixCases s result) (Proxy l) Unit a (Variant result) where
  foldingWithIndex _ _ _ a = Variant.inj (Proxy :: Proxy l') a

type NilExpr
  = Lift (RowList.Nil ∷ RowList Type)

add ::
  forall rin rout pre.
  IsSymbol pre =>
  Eval ((ToRow <<< FoldrWithIndex (PrefixStep pre) NilExpr <<< FromRow) rin) rout =>
  HFoldlWithIndex (PrefixCases pre rout) Unit (Variant rin) (Variant rout) =>
  Proxy pre ->
  Variant rin ->
  Variant rout
add _ = hfoldlWithIndex (PrefixCases :: PrefixCases pre rout) unit

remove ::
  forall pre rin rout.
  Eval ((ToRow <<< FoldrWithIndex (UnprefixStep pre) NilExpr <<< FromRow) rin) rout =>
  HFoldlWithIndex (UnprefixCases pre rout) Unit (Variant rin) (Variant rout) =>
  Proxy pre ->
  Variant rin ->
  Variant rout
remove _ = hfoldlWithIndex (UnprefixCases :: UnprefixCases pre rout) unit
