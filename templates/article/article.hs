{-# LANGUAGE QuasiQuotes, OverloadedStrings, UnicodeSyntax #-}
{-# OPTIONS_GHC -F -pgmF frquotes #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-unused-imports #-}

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
import qualified Language.LaTeX.Builder.Math as M
import qualified Language.LaTeX.Length as L
import Language.LaTeX.Slicer (slice,(^$))
import Language.LaTeX.Builder.QQ

import Kit

-- The document class of your document
docclass = B.article (Just (L.pt 11)) (Just B.a4paper) []

-- Some preliminary declarations
preamble = B.title     «My nice article»
         ⊕ B.author    «Me and some others»
      -- ⊕ [$qp|Some raw \LaTeX code in the preamble|]

body = slice . execWriter $ do
  maketitle

  section «Introduction»

  p «Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod
    tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam,
    quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo
    consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse
    cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat
    non proident, sunt in culpa qui officia deserunt mollit anim id est
    laborum.»

  section «Bla»

  p [$tex|Here is some raw \LaTeX code, use it with care.|]

  subsection «Foo»

doc = B.document docclass preamble body

main = quickView myViewOpts{basedir="out",showoutput=False,pdfviewer="evince"} "article" doc
