import qualified Language.LaTeX.Builder as B
import Language.LaTeX.Printer (ppRoot)

import Data.Monoid

infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend

exs = putStrLn s >> writeFile "tests/test.ltx" s
  where s = ppRoot ex ""

ex = B.root expreamb exdoc

expreamb = B.documentclass (Just (B.pt 11)) (Just B.a4paper) B.book

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
               ,B.item (B.pstring "Convince people around that this one is much better")]
  <> B.chapter (B.hstring "Related Works")
