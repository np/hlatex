{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
import Language.LaTeX
import qualified Language.LaTeX.Builder as B
import Data.String ()

main = quickView testViewOpts "indented-itemizes" root

root = B.root preamb $ B.document doc

preamb = B.documentclass (Just (B.pt 11)) (Just B.a4paper) B.article

doc = mk "bla bla:" ["Aaaa", "Bbbb", "Cccc"]
   <> mk "bli bli:" ["Dddd", "Eeee", "Ffff"]

mk :: LatexItem -> [LatexItem] -> ParItem
-- mk heading contents = B.para heading <> B.itemize (map (B.item . B.para) contents)
mk heading (item0 : items) = B.tabular [B.l, B.l] (B.cells [heading, bullet <> item0]
                                                  :map mkrow items)
  where mkrow item = B.cells [ø, bullet <> item]
        bullet = "- "
