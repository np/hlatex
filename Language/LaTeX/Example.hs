{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-missing-signatures #-}
import Language.LaTeX.Types (runLatexM)
import qualified Language.LaTeX.Builder as B
import Language.LaTeX.Builder ((!<), (<!), (!<!))
import Language.LaTeX.Printer (ppRoot)
import System.Cmd (system)

import Data.Char
import Data.Monoid
import Data.List.Split
import Data.List (intersperse)
import Control.Monad.Writer (tell, execWriter)

infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend

exs = putStrLn s >> writeFile "tests/test.ltx" s >> system "(cd tests && pdflatex test.ltx && open test.pdf)"
  where s = ppRoot (either error id $ runLatexM ex) ""

ex = B.root expreamb exdoc

expreamb = B.documentclass (Just (B.pt 11)) (Just B.a4paper) B.book
        <> B.usepackage "amsmath"

exdoc = B.document $
     B.tableofcontents
  <> B.part "The prologue"
  <> B.chapter "Introduction"
  <> B.section "The context"
  <> B.subsection "The precise context"
  <> B.para ("The " <> B.textbf "initial" <> " formula was " <>
             B.math (B.sum <> B.sub "i = 0" <> B.sup B.infty <>
                     "i" <> B.sup (B.mint 2) <> B.alpha) <>
             " but it turns out to be not that accurate.")
  <> B.section ("The action plan")
  <> B.itemize [B.item "Find a better formula"
               ,B.item "Write some proofs about it"
               ,B.item "[I'm not a label] of this item"
               ,B.item' "label here" "item text"
               ,B.item "Convince people around that this one is much better"]
  <> B.chapter "Related Works"
  <> B.tabular [B.c,B.l,B.r]
       [ B.cells $ map B.math [B.alpha + 3 * B.beta, B.eq, 2 * 21]
       , B.hline, B.hline
       , B.cells [mempty, B.math B.eq, B.math 42]
       , B.cline 2 3
       ]
  <> B.displaymath
       (B.array [B.vline,B.l,B.vline,B.vline,B.c,B.vline,B.r,B.vline]
                [B.hline
                ,B.cells [1, 2, 3]
                ,B.cells [ B.sin <> (B.pi / 2), B.frac (B.cos <> B.gamma) B.epsilon
                         , B.sum <> B.sub "i" <> B.sup B.infty <> "i" <> B.sup "i"]
                ,B.hline]
       )
  <> B.displaymath (B.brackets mat33)
  <> B.displaymath (B.parens mat33)
  <> B.displaymath (B.braces mat33)
  <> B.displaymath (B.between '(' '[' mat33)
  <> B.displaymath (B.array [B.c,B.c,B.c]
       [B.cells [B.text "f x = "
                ,B.between '{' '.' (B.array [B.l] (map B.cell [1, 0]))
                ,B.array [B.r] (map (B.cell . B.text) ["if x is positive", "otherwise"])
                ]
       ])
  <> B.displaymath (B.sqrt' B.alpha B.beta)
  <> B.newpage
  <> B.para (B.noindent <> (B.texttt $ mconcat $ intersperse B.newline $ map B.protect (splitEvery 10 $ filter isPrint $ map chr [0..255])))
  <> B.section "Let's try the Writer monad to write documents"
  <> (execWriter $ do
       tell $ B.subsection $ "execWriter, tell and (!<!)"
       tell $ B.description $
         let doNotation = B.texttt "do" <> " notation" in
         [ B.item' "execWriter" $ B.para $
             "This function runs the writing computation and returns       \
             \the accumulated value. Typically when you want to use        \
             \the "<>doNotation<>", you can start using execWriter."
         , B.item' "tell" $ B.para $
             "This function accumulate the given value, this is commonly used \
             \inside the "<>doNotation<>"."
         , B.item' "(!<!)" $ B.para $
             "This function combines "<>B.texttt "execWriter"<>" and "<>B.texttt "tell"<>
             " to be easily used when building documents with the "<>doNotation<>"."
         ]
       B.center !<! do
         B.para !< "Frist centered paragraph"
         B.para !< "Second centered paragraph"
     )

  where mat33 = B.array (replicate 3 B.c) (map B.cells [[1,B.cdots,3],[B.vdots,B.ddots,B.vdots],[4,B.cdots,6]])
