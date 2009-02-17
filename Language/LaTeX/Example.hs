{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-missing-signatures #-}
import Language.LaTeX.Types (runLatexM)
import qualified Language.LaTeX.Builder as B
import qualified Language.LaTeX.Builder.Math as M
import qualified Language.LaTeX.Builder.Graphics as G
import Language.LaTeX.Builder ((!<), (<!), (!<!))
import Language.LaTeX.Printer (ppRoot)
import System.Cmd (system)

import Data.Ratio ((%))
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
        <> B.usepackage "graphicx"

exdoc = B.document $
     B.tableofcontents
  <> B.part "The prologue"
  <> B.chapter "Introduction"
  <> B.section "The context"
  <> B.subsection "The precise context"
  <> B.para ("The " <> B.textbf "initial" <> " formula was " <>
             B.math (M.sum <> M.sub (M.i<>M.eq<>0) <> M.sup M.infty <>
                     M.i <> M.sup 2 <> M.alpha) <>
             " but it turns out to be not that accurate.")
  <> B.section ("The action plan")
  <> B.itemize [B.item $ B.para "Find a better formula"
               ,B.item $ B.para "Write some proofs about it"
               ,B.item $ B.para "[I'm not a label] of this item"
               ,B.item' "label here" $ B.para "item text"
               ,B.item $ B.para "Convince people around that this one is much better"]
  <> B.chapter "Related Works"
  <> B.tabular [B.c,B.l,B.r]
       [ B.cells $ map B.math [M.alpha + 3 * M.beta, M.eq, 2 * 21]
       , B.hline, B.hline
       , B.cells [mempty, B.math M.eq, B.math 42]
       , B.cline 2 3
       ]
  <> B.displaymath
       (M.array [B.vline,B.l,B.vline,B.vline,B.c,B.vline,B.r,B.vline]
                [B.hline
                ,B.cells [1, 2, 3]
                ,B.cells [ M.sin <> (M.pi / 2), M.frac (M.cos <> M.gamma) M.epsilon
                         , M.sum <> M.sub M.i <> M.sup M.infty <> M.i <> M.sup M.i]
                ,B.hline]
       )
  <> B.displaymath (M.brackets mat33)
  <> B.displaymath (M.parens mat33)
  <> B.displaymath (M.braces mat33)
  <> B.displaymath (M.between '(' '[' mat33)
  <> B.displaymath (M.array [B.c,B.c,B.c]
       [B.cells [M.text "f x = "
                ,M.between '{' '.' (M.array [B.l] (map B.cell [1, 0]))
                ,M.array [B.r] (map (B.cell . M.text) ["if x is positive", "otherwise"])
                ]
       ])
  <> B.displaymath (M.sqrt' M.alpha M.beta)
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
  <> B.section "Exotic tabular features"
  <> B.tabular [B.r, B.rtext "@", B.l, B.rtext (B.math M.alpha), B.c]
               (map B.cells [["x","y","z"],["foo", "bar", "baz"],["a", "b", "c"]])
  <> B.displaymath
     (M.array [B.r, B.rtext M.vdots, B.l, B.rtext M.alpha, B.c]
               (map B.cells [[M.x,M.y,M.z],[1,2,3],[M._R, M._C, M._N]]))
  <> G.includegraphics (\r-> r{G.angle=45, G.scale=1%2}) "yi.pdf"

  where mat33 = M.array (replicate 3 B.c) (map B.cells [[1,M.cdots,3],[M.vdots,M.ddots,M.vdots],[4,M.cdots,6]])
