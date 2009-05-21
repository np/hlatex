module Language.LaTeX.Builder.MonoidUtils ((<>), mconcatMap, mapNonEmpty) where

import Data.Monoid

infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend

mconcatMap :: Monoid m => (a -> m) -> [a] -> m
mconcatMap f = mconcat . map f

mapNonEmpty :: (Eq a, Monoid a, Monoid b) => (a -> b) -> a -> b
mapNonEmpty f x | x == mempty = mempty
                | otherwise   = f x

