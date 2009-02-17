module Language.LaTeX.Builder.MonoidUtils ((<>)) where

import Data.Monoid

infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend

