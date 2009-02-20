module Language.LaTeX.Builder.MonoidUtils ((<>), mconcatMap) where

import Data.Monoid

infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend

mconcatMap :: Monoid m => (a -> m) -> [a] -> m
mconcatMap f = mconcat . map f