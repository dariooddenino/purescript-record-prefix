module Data.Variant.Prefix where

import Prelude
import Data.Variant (Variant, inj)
import Data.Variant (inj) as Variant
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Prim.Row (class Cons) as Row
import Prim.RowList (Cons) as RowList
import Prim.Symbol (class Append) as Symbol
import Type.Eval (class Eval, kind TypeExpr)
import Type.Eval.Foldable (FoldrWithIndex)
import Type.Eval.Function (type (<<<))
import Type.Eval.RowList (FromRow, ToRow)
import Type.Prelude (class IsSymbol, RLProxy, RProxy, SProxy(..))

foreign import data PrefixStep :: Symbol -> Type -> Type -> TypeExpr -> TypeExpr

instance evalPerfixStep ::
  ( Symbol.Append prefix label label'
  , Eval tail (RLProxy tail')
  , IsSymbol label'
  ) =>
  Eval (PrefixStep prefix (SProxy label) value tail) (RLProxy (RowList.Cons label' value tail'))

data PrefixCases (s :: Symbol) (result :: # Type)
  = PrefixCases

instance prefixCases ::
  ( Symbol.Append s l l'
  , IsSymbol l'
  , Row.Cons l' a result_ result
  ) =>
  FoldingWithIndex (PrefixCases s result) (SProxy l) Unit a (Variant result) where
  foldingWithIndex _ prop _ a = Variant.inj (SProxy :: SProxy l') a

foreign import data UnprefixStep :: Symbol -> Type -> Type -> TypeExpr -> TypeExpr

instance evalunprefixStep ::
  ( Symbol.Append prefix label' label
  , Eval tail (RLProxy tail')
  , IsSymbol label'
  ) =>
  Eval (UnprefixStep prefix (SProxy label) value tail) (RLProxy (RowList.Cons label' value tail'))

data UnprefixCases (s :: Symbol) (result :: # Type)
  = UnprefixCases

instance unprefixCases ::
  ( Symbol.Append s l' l
  , IsSymbol l'
  , Row.Cons l' a result_ result
  ) =>
  FoldingWithIndex (UnprefixCases s result) (SProxy l) Unit a (Variant result) where
  foldingWithIndex _ prop _ a = Variant.inj (SProxy :: SProxy l') a

-- | Evaluation of this `TypeExpr` gives as back a `RowList.Nil`
type NilExpr
  = FromRow (RProxy ())

add ::
  forall rin rout pre.
  IsSymbol pre =>
  Eval ((ToRow <<< FoldrWithIndex (UnprefixStep pre) NilExpr <<< FromRow) (RProxy rin)) (RProxy rout) =>
  HFoldlWithIndex (PrefixCases pre rout) Unit (Variant rin) (Variant rout) =>
  SProxy pre ->
  Variant rin ->
  Variant rout
add p = hfoldlWithIndex (PrefixCases :: PrefixCases pre rout) unit

remove ::
  forall pre rin rout.
  Eval ((ToRow <<< FoldrWithIndex (UnprefixStep pre) NilExpr <<< FromRow) (RProxy rin)) (RProxy rout) =>
  HFoldlWithIndex (UnprefixCases pre rout) Unit (Variant rin) (Variant rout) =>
  SProxy pre ->
  Variant rin ->
  Variant rout
remove p = hfoldlWithIndex (UnprefixCases :: UnprefixCases pre rout) unit

-- | No annotation needed for intermediate result
test = remove (SProxy :: SProxy "b") (remove (SProxy :: SProxy "foo") v)
  where
  v :: Variant ( foobar :: Int )
  v = inj (SProxy :: SProxy "foobar") 8
