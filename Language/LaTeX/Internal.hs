module Language.LaTeX.Internal ((<>)) where

import Data.Monoid

infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
