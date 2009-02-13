{-# OPTIONS -fno-warn-missing-signatures #-}
import qualified Language.LaTeX.Builder as B
import Language.LaTeX.Printer (ppRoot)
import System.Cmd (system)

import Data.Char
import Data.Monoid
import Data.List.Split
import Data.List (intersperse)

infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend

exs = putStrLn s >> writeFile "tests/test.ltx" s >> system "(cd tests && pdflatex test.ltx && open test.pdf)"
  where s = ppRoot ex ""

ex = B.root expreamb exdoc

expreamb = B.documentclass (Just (B.pt 11)) (Just B.a4paper) B.book
        <> B.usepackage (B.hstring "amsmath")

exdoc = B.document $
     B.part (B.hstring "The prologue")
  <> B.chapter (B.hstring "Introduction")
  <> B.section (B.hstring "The context")
  <> B.subsection (B.hstring "The precise context")
  <> B.para (B.hstring "The initial formula was " <>
             B.maths (B.sum <> B.sub (B.mstring "i = 0") <> B.sup (B.infty) <>
                      B.mstring "i" <> B.sup (B.mint 2) <> B.alpha) <>
             B.hstring " but it turns out to be not that accurate.")
  <> B.section (B.hstring "The action plan")
  <> B.itemize [B.item (B.para $ B.hstring "Find a better formula")
               ,B.item (B.pstring "Write some proofs about it")
               ,B.item (B.pstring "[I'm not a label] of this item")
               ,B.item' (B.hstring "label here") (B.pstring "item text")
               ,B.item (B.pstring "Convince people around that this one is much better")]
  <> B.chapter (B.hstring "Related Works")
  <> B.tabular [B.c,B.l,B.r]
       [ B.cells [B.maths (B.alpha + 3 * B.beta), B.maths B.eq, B.maths (2 * 21)]
       , B.hline, B.hline
       , B.cells [mempty                        , B.maths B.eq, B.maths 42]
       , B.cline 2 3
       ]
  <> B.displaymath
       (B.array [B.vline,B.l,B.vline,B.vline,B.c,B.vline,B.r,B.vline]
                [B.hline
                ,B.cells [1, 2, 3]
                ,B.cells [ B.sin <> (B.pi / 2), B.frac (B.cos <> B.gamma) B.epsilon
                         , let i = B.mchar 'i' in B.sum <> B.sub i <> B.sup B.infty <> i <> B.sup i]
                ,B.hline]
       )
  <> B.displaymath (B.brackets mat33)
  <> B.displaymath (B.parens mat33)
  <> B.displaymath (B.braces mat33)
  <> B.displaymath (B.between '(' '[' mat33)
  <> B.displaymath (B.array [B.c,B.c,B.c]
       [B.cells [B.text (B.hstring "f x = ")
                ,B.between '{' '.' (B.array [B.l] (map (B.cells . (:[])) [1, 0]))
                ,B.array [B.r] (map (B.cells . (:[]) . B.text . B.hstring) ["if x is positive", "otherwise"])
                ]
       ])
  <> B.displaymath (B.sqrt' B.alpha B.beta)
  <> B.newpage
  <> B.para (B.noindent <> (B.texttt $ mconcat $ intersperse B.newline $ map B.protect (splitEvery 10 $ filter isPrint $ map chr [0..255])))

  where mat33 = B.array (replicate 3 B.c) (map B.cells [[1,B.cdots,3],[B.vdots,B.ddots,B.vdots],[4,B.cdots,6]])
