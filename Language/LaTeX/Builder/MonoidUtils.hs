module Language.LaTeX.Builder.MonoidUtils (ø, (⊕), (<>), (<||>), (<&&>), mapNonEmpty) where

import Data.Monoid
import Data.Monoid.Unicode ((⊕))

infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend

ø :: Monoid m => m
ø = mempty

(<||>) :: (Monoid a, Eq a) => a -> a -> a
a <||> b | a == ø     = b
         | otherwise  = a

(<&&>) :: (Monoid a, Eq a, Monoid b) => a -> b -> b
a <&&> b | a == ø     = ø
         | otherwise  = b

mapNonEmpty :: (Eq a, Monoid a, Monoid b) => (a -> b) -> a -> b
mapNonEmpty f x | x == ø     = ø
                | otherwise  = f x

