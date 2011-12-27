{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}
import Language.LaTeX
import qualified Language.LaTeX.Builder as B
import qualified Language.LaTeX.Builder.Babel as B
import qualified Language.LaTeX.Length as L
import Data.List
import Data.Monoid.Unicode

main = printLatexDocument body

preamb = B.useBabel B.francais []
       ⊕ B.useInputenc B.utf8

texLines = mconcat . intersperse B.newline . filter (/= mempty)

author = texLines ["Nicolas POUILLARD"
                  ,"01 23 45 67 89"
                  ,"42, rue Foo"
                  ,"00000 Quelque part sur Seine"
                  ]

body = B.document (B.letter []) preamb . mconcat $
           [B.para author
           ,B.vfill
           ,B.para "Le Mardi 22 Juin à Paris"
           ,B.vspace (L.em 3)
           ,B.para . mconcat . unwords $
             replicate 100 "Bla"
           ,B.vfill
           ]
