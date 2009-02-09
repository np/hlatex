import qualified Language.LaTeX.Builder as B
import Language.LaTeX.Printer (ppRoot)

exs = putStrLn s >> writeFile "tests/test.ltx" s
  where s = ppRoot ex ""

ex = B.root expreamb [exdoc]

expreamb = [B.documentclass (Just (B.pt 11)) (Just B.a4paper) B.book]

exdoc = B.document [] [
    B.part [B.string "The prologue"]
  , B.chapter [B.string "Introduction"]
  , B.section [B.string "The context"]
  , B.subsection [B.string "The precise context"]
  , B.paragraph [ B.string "The initial formula was "
                , B.maths [B.sum, B.sub [B.mstring "i = 0"], B.sup [B.infty]
                          , B.mstring "i", B.sup [B.mint 2], B.alpha]
                , B.string " but it turns out to be not that accurate."]
  , B.section [B.string "The action plan"]
  , B.itemize [B.item [B.string "Find a better formula"]
              ,B.item [B.string "Write some proofs about it"]
              ,B.item [B.string "Convince people around that this one is much better"]
              ]
  , B.chapter [B.string "Related Works"]
  ]