{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
import Language.LaTeX
import qualified Language.LaTeX.Builder as B
import qualified Language.LaTeX.Builder.Math as M
import qualified Language.LaTeX.Builder.Graphics as G
-- import qualified Language.LaTeX.Builder.Rotating as R
-- import qualified Language.LaTeX.Builder.Color as C

import Data.Ratio ((%))
import Data.Char
import Data.List.Split
import Data.List

docName = "FILL ME"

main = quickView testViewOpts docName root

root = B.root preamb body

preamb = B.documentclass (Just (B.pt 11)) (Just B.a4paper) B.book

body = B.document <!
  tell B.tableofcontents
