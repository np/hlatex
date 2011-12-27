module Language.LaTeX.Class where

import Language.LaTeX.Types

class ToLatexItem a where
  toLatexItem :: a -> LatexItem

class ToParItem a where
  toParItem :: a -> ParItem

class ToPreambleItem a where
  toPreambleItem :: a -> PreambleItem

class ToMathItem a where
  toMathItem :: a -> MathItem

class ToAnyItem a where
  toAnyItem :: a -> AnyItem

instance ToLatexItem LatexItm where
  toLatexItem = return

foldToLatexItem :: (ToLatexItem a, Foldable f) => f a -> LatexItem
foldToLatexItem = foldMap toLatexItem

instance ToLatexItem a => ToLatexItem [a] where
  toLatexItem = foldToLatexItem
