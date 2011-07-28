{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
import Language.LaTeX
import qualified Language.LaTeX.Builder as B
import qualified Language.LaTeX.Builder.Internal as BI
import qualified Language.LaTeX.Builder.Math as M
import qualified Language.LaTeX.Builder.Graphics as G
import qualified Language.LaTeX.Builder.Rotating as R
import qualified Language.LaTeX.Builder.Color as C
import qualified Language.LaTeX.Length as L

import Data.Ratio ((%))
import Data.Char
import Data.List.Split
import Data.List (intersperse)

main = quickView testViewOpts "test" doc

paraNoindent = B.para . (B.noindent ⊕)

doc = B.document dc preamb body
  where
    dc = B.book (Just (L.pt 11)) (Just B.a4paper) []
    preamb = ø
          -- B.usepackage [B.optional "francais"] (B.pkgName "babel")

-- lam x t = "λ"⊕x⊕". "⊕t
-- app t u = ...
-- var x = ...

-- rep :: Int -> LatexItem -> LatexItem
-- rep x y = mconcat (replicate x y)

-- latexToInt :: LatexItem -> Int
-- latexToInt (LatexLength ...)

-- rep' :: LatexItem -> LatexItem -> LatexItem
-- rep' x y = mconcat (replicate (latexToInt x) y)

newcommand nargs name body = latexCmd "newcommand" name [optional nargs, mandatory (body $ map (('#':).show) [1..nargs-1])]

newcommand2 name body = newcommand name (\[x,y] -> body x y)

lib = newcommand "lam" lam ⊕ ...

body = id $? do
  tell B.tableofcontents
  B.part !$ "The prologue"

  B.chapter' (Just "Introduction (toc)") !$ "Introduction (not toc)"

  B.section' (Just "") !$ "The context"

  B.subsection !$ "The precise context"

  B.para !$ ("The " ⊕ B.textbf "initial" ⊕ " formula was " ⊕
             B.math (M.sum ⊕ M.sub (M.i⊕M.eq⊕0) ⊕ M.sup M.infty ⊕
                     M.i ⊕ M.sup 2 ⊕ M.alpha) ⊕
             " but it turns out to be not that accurate.")

  B.section !$ "The action plan"

  B.paragraph !$ "Here comes an itemize"

  B.itemize ø !$
     [ B.item $ B.para "Find a better formula"
     , B.item $ B.para "Write some proofs about it"
     , B.item $ B.para "[I'm not a label] of this item"
     , B.item' "label here" $ B.para "item text"
     , B.item $ B.para "Convince people around that this one is much better"
     ]

  B.section !$ "Related Works"

  B.tabular [B.c,B.l,B.r] !$
       [ B.cells $ map B.math [M.alpha + 3 * M.beta, M.eq, 2 * 21]
       , B.hline, B.hline
       , B.cells [ø, B.math M.eq, B.math 42]
       , B.cline 2 3
       ]

  B.displaymath !$
    M.array [B.vline,B.l,B.vline,B.vline,B.c,B.vline,B.r,B.vline]
            [ B.hline
            , B.cells [1, 2, 3]
            , B.cells [ M.sin ⊕ (M.pi / 2), M.frac (M.cos ⊕ M.gamma) M.epsilon
                      , M.sum ⊕ M.sub M.i ⊕ M.sup M.infty ⊕ M.i ⊕ M.sup M.i]
            , B.hline]

  B.displaymath !$ M.brackets mat33

  B.displaymath !$ M.parens mat33

  B.displaymath !$ M.braces mat33

  B.displaymath !$ M.between '(' '[' mat33

  B.displaymath !$ M.array [B.c,B.c,B.c]
       [B.cells [M.text "f x = "
                ,M.between '{' '.' (M.array [B.l] (map B.cell [1, 0]))
                ,M.array [B.r] (map (B.cell . M.text) ["if x is positive", "otherwise"])
                ]
       ]

  B.displaymath !$ M.sqrt' M.alpha M.beta

  tell B.newpage

  let letters = splitEvery 10 $ filter isPrint $ map chr [0..255]
  paraNoindent !$ B.texttt . mconcat . intersperse (B.newline ø) $ map B.protect letters
  paraNoindent !$ mconcat . intersperse (B.newline ø) $ map B.hstring letters
  paraNoindent !$ mconcat . intersperse (B.newline ø) $ map B.verb letters

  B.section !$ "Let's try the Writer monad to write documents"

  B.subsection !$ "execWriter, tell and (!$?)"
  B.description ø !$
    let doNotation = B.texttt "do" ⊕ " notation" in
    [ B.item' "execWriter" . B.para $
        "This function runs the writing computation and returns       \
        \the accumulated value. Typically when you want to use        \
        \the "⊕doNotation⊕", you can start using execWriter."
    , B.item' "tell" . B.para $
        "This function accumulate the given value, this is commonly used \
        \inside the "⊕doNotation⊕"."
    , B.item' "(!$?)" . B.para $
        "This function combines "⊕B.texttt "execWriter"⊕" and "⊕B.texttt "tell"⊕
        " to be easily used when building documents with the "⊕doNotation⊕"."
    ]

  B.center !$? do
    B.para !$ "Frist centered paragraph"
    B.para !$ "Second centered paragraph"

  B.section !$ "Exotic tabular features"

  B.tabular [B.r, B.rtext "@", B.l, B.rtext (B.math M.alpha), B.c] !$
               map B.cells [["x","y","z"],["foo", "bar", "baz"],["a", "b", "c"]]

  B.displaymath !$
     M.array [B.r, B.rtext M.vdots, B.l, B.rtext M.alpha, B.c]
             (map B.cells [[M.x,M.y,M.z],[1,2,3],[M._R, M._C, M._N]])

  -- G.includegraphics (\r-> r{G.angle=45, G.scale=1%2}) !$ "yi.pdf"

  B.para !$ (B.noindent⊕B.decl B._Large ("Not shelfful"⊕B.newline ø⊕"but shelf"⊕B.sep⊕"ful"))

  B.para !$ "This is some|text"
  B.para !$ "This is some-text"
  B.para !$ "This is some--text"
  B.para !$ BI.rawTex "This is some|text"
  B.para !$ "This is some---text"
  B.para !$ ("This is some"⊕B.dash1⊕"text")
  B.para !$ ("This is some"⊕B.dash2⊕B.dash1⊕"text")
  B.para !$ ("This is some"⊕B.dash3⊕"text")
  B.para !$ "This is some``text"
  B.para !$ "This is ''some``text"
  B.para !$ "This is ''''some``text"
  B.para !$ "This is ''''some````text"
  B.para !$ "This is ''``''''````''``"
  B.para !$ "This is some``text''"
  B.para !$ "This is so's'"
  B.para !$ "This is so's''"
  B.para !$ "This is so`s`"
  B.para !$ "This is so`s``"

  B.para !$? do
    B.circ !$ "o"
    B.tilde !$ B.i
    B.grave !$ "e"
    B.check !$ B.j
    B.grave !$ "z"
    B.acute !$ "y"
    B.uml !$ "e"
    B.cedil !$ "a"
    B.ring !$ "u"
    B.dot !$ "o"
    B.tieafter !$ "uv"
    B.overbar !$ "f"
    B.overdot !$ "c"
    B.underbar !$ B.ae
    tell . B.math $ mconcat
      [ M.acute M.alpha
      , M.breve M.beta
      , M.check M.delta
      , M.dot M.gamma
      , M.ddot M.zeta
      , M.vec M.iota
      ]

  tell . B.para $ B.unwords
    [ B._LaTeX
    , B._TeX
    , B.copyright
    , B.dag
    , B.ddag
    , B.ldots
    , B.lq
    , B.rq
    , B._P
    , B._S
    , B.pounds
    ]

  B.tabular [B.vline, B.c, B.vline, B.c, B.vline] !$
    [B.hline
    ,B.cells (map (R.turn 90) ["foo", "bar"])
    ,B.hline
    ,B.cells [C.textcolor C.red "foo", C.colorbox C.blue "bar"]
    ,B.hline
    ]

  where mat33 = M.array (replicate 3 B.c) (map B.cells [[1,M.cdots,3],[M.vdots,M.ddots,M.vdots],[4,M.cdots,6]])
