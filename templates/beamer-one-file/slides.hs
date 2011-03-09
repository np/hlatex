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

-- Most of these are imported qualified, then
-- most used combinators are locally defined
-- (near the bottom of the file)
import Language.LaTeX
import qualified Language.LaTeX.Builder as B
import qualified Language.LaTeX.Builder.Internal as BI
import qualified Language.LaTeX.Builder.Beamer as BM
import qualified Language.LaTeX.Builder.Math as M
import qualified Language.LaTeX.Length as L
import Language.LaTeX.Slicer (slice,(^$))
import Language.LaTeX.Builder.QQ

doc = B.document docclass preamb body

docclass = BM.beamer Nothing [BM.t,BM.compress] []

preamb = B.title     «My fancy slides»
       ⊕ B.author    «Me and some others»
       ⊕ B.institute «Foo bar institute»
       ⊕ BM.beamertemplatenavigationsymbolsempty
       ⊕ BM.useoutertheme [] "default"
       ⊕ BM.useinnertheme [("shadow","true")] "rounded"
       ⊕ BM.usecolortheme [] "orchid"
       ⊕ margins

body = slice . execWriter $ do
  put B.maketitle

  slide «Writing slides» $ do -- <--- Change this $ to ^$ in order to compile only this slide
    p «First itemize»
    itemize $ do
      item «First item»
      item «Second item»
    p «Second itemize»
    itemize $ do
      item «First item»
      item «Second item»

  slideCB «Big news»

  slide «A sentence and an example» $ do
    p «The following examples»
    example . unlines $
      ["foo :: Bar"
      ,"foo x = x"
      ]

  slide «Descriptions» $ do
    description $ do
      itemD «Item one:» «bla bla bla»
      itemD «Item two:» «bli bla blo»

todo :: a -> a
todo = id
{-# DEPRECATED todo "You have something to do here" #-}

ø :: Monoid m => m
ø = mempty

main = quickView myViewOpts{basedir="out",showoutput=False,pdfviewer="evince"} "slides" doc

usepackages = mconcat . map (BI.usepackage [] . BI.pkgName)

p = put . B.para

vcenter x = B.vfill ⊕ x ⊕ B.vfill

put :: ParItem -> ParItemW
put = tell

slide title = put . BM.slide title . mapNonEmpty vcenter . execWriter

margins = BM.setbeamersize (BM.TextMarginLeft (L.cm 0.3))
        ⊕ BM.setbeamersize (BM.TextMarginRight (L.cm 0.3))

slideCs title subtitle =
    slide ø . put . B.center . (⊕subt) . B.para $ title
  where subt = mapNonEmpty ((B.vfill ⊕) . B.para) subtitle

slideCB x = slideCs (B.decl B._Huge x) ø

verb = B.texttt . B.protector (myXchar (M.mchar B.ttchar))

-- let's customize the rendering of some charcters in `verb' mode
myXchar xchar x
  | x `elem` "_"   = B.makebox (L.ex 1.22) Centered (xchar x)
  | x `elem` "="   = B.ttchar x
  | x `elem` "{}"  = M.mchar B.hchar x
myXchar xchar x    = xchar x

code = verb . dropWhile (=='\n')

example :: String -> ParItemW
example = put . BM.block ø . B.para . code

itemize block = B.itemize !$? block
description block = B.description !$? block
item = tell . return . B.item . B.para
itemD x = tell . return . B.item' x . B.para
