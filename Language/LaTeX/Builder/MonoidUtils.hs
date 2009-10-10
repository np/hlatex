module Language.LaTeX.Builder.MonoidUtils ((<>), (<||>), (<&&>), mapNonEmpty) where

import Data.Monoid

infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend

(<||>) :: (Monoid a, Eq a) => a -> a -> a
a <||> b | a == mempty = b
         | otherwise   = a

(<&&>) :: (Monoid a, Eq a, Monoid b) => a -> b -> b
a <&&> b | a == mempty = mempty
         | otherwise   = b

mapNonEmpty :: (Eq a, Monoid a, Monoid b) => (a -> b) -> a -> b
mapNonEmpty f x | x == mempty = mempty
                | otherwise   = f x

