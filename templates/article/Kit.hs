{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF frquotes #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-unused-imports #-}
module Kit where

-- The writer monad is used to get the "do" notation
-- for writing environments like slides or itemizes.
import Control.Monad.Writer

import Control.Applicative

-- String literals are overloaded. This module contains the fromString
-- function for example.
import Data.String

-- Getting ⊕ (ø is defined below)
import Data.Monoid.Unicode ((⊕))

import Language.LaTeX
import qualified Language.LaTeX.Builder as B
import qualified Language.LaTeX.Builder.Internal as BI
import qualified Language.LaTeX.Builder.Math as M
import qualified Language.LaTeX.Length as L
import Language.LaTeX.Slicer (slice,(^$))
import Language.LaTeX.Builder.QQ

todo :: a -> a
todo = id
{-# DEPRECATED todo "You have something to do here" #-}

usepackage xs = BI.usepackage xs . BI.pkgName
usepackages = mconcat . map (usepackage [])

vcenter x = B.vfill ⊕ x ⊕ B.vfill

verb = B.texttt . B.protector (myXchar (M.mchar B.ttchar))

-- let's customize the rendering of some charcters in `verb' mode
myXchar xchar x
  | x `elem` "_"   = B.makebox (L.ex 1.22) B.centered (xchar x)
  | x `elem` "="   = B.ttchar x
  | x `elem` "{}"  = M.mchar B.hchar x
myXchar xchar x    = xchar x

code = verb . dropWhile (=='\n')

-- Monadic style

put :: ParItem -> ParItemW
put = tell
p = put . B.para
paragraph = put . B.paragraph
itemize block = B.itemize ø !$? block
description block = B.description ø !$? block

item :: (MonadWriter (f ListItem) m, Monad f) => LatexItem -> m ()
item = tell . return . B.item . B.para

itemD :: (MonadWriter (f ListItem) m, Monad f) => LatexItem -> LatexItem -> m ()
itemD x = tell . return . B.item' x . B.para

maketitle = put B.maketitle
section = put . B.section
subsection = put . B.subsection
