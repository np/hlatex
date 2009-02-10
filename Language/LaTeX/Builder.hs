{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fno-warn-missing-signatures #-}
module Language.LaTeX.Builder where

import Prelude hiding (and)
import Data.List (intersperse)
import Data.Maybe
import Data.Ratio
import Data.Monoid
import Control.Arrow

import Language.LaTeX.Types
import Language.LaTeX.Data
import Language.LaTeX.Internal
import Language.Haskell.TH

$(
 let
  mkMathsCmd (name, _) = 
    let lname = lowerName name
        upperMCName = mkName ("MC"++name)
    in
    [sigD lname [t| MathsItem |]
    ,valD (varP lname) (normalB [| MathsCmd $(conE upperMCName) |]) []
    ]

  mkMathsCmdArg name = 
    let lname = lowerName name in
    [sigD lname [t| MathsItem -> MathsItem |]
    ,valD (varP lname) (normalB [| MathsCmdArg $(stringE name) |]) []
    ]

  mkTexCmd name = 
    let lname = lowerName name in
    [sigD lname [t| Latex |]
    ,valD (varP lname) (normalB [| TexCmd $(stringE name) |]) []
    ]

  mkLatexCmd name = 
    let lname = lowerName name in
    [sigD lname [t| Latex -> Latex |]
    ,valD (varP lname) (normalB [| LatexCmd $(stringE name) |]) []
    ]

 in sequence $ concat $ concat
      [ map mkMathsCmd mathsCmds
      , map mkMathsCmdArg mathsCmdsArg
      , map mkTexCmd texCmds
      , map mkLatexCmd latexCmds
      ]
 )

texgroup = TexGroup

amp = RawTex " & "

maths = MathsInline
mathsBlock = MathsInline

mstring = RawMaths . concatMap mchar
mint = MathsInt

sub x = MathsConcat [RawMaths "_", MathsGroup x]
sup x = MathsConcat [RawMaths "^", MathsGroup x]


href x y = LatexCmdArgs "href" [x,y]
person name email = href (string ("mailto:"++email)) (string name)

pt = Pt
-- em = Em
cm = Cm

rule x y = LatexCmdArgs "rule" [x,LatexSize y]

-- simulate the <hr> html tag
hrule = texgroup $ noindent <> rule linewidth (pt 1.5)

normSpaces = unlines . map (unwords . words) . lines

string = RawTex . concatMap char

char '\\' = "\backslash"
char '~' = "$\\tilde{}$"
char '<' = "\\textless{}"
char '>' = "\\textgreater{}"
char '|' = "\\textbar{}"
char ':' = "$:$"
char c | c `elem` "#_&{}$%" = ['\\',c]
       | otherwise          = [c]

mchar '\\' = "\backslash"
mchar '~' = "\\tilde{}"
mchar ':' = ":"
mchar c | c `elem` "#_&{}$%" = ['\\',c]
        | otherwise          = [c]

protect :: String -> [Latex]
protect ""        = []
protect ('\n':cs) = newline : protect cs
protect (' ':cs)  = uncurry (++) $ (hspace_ . (+1) . length *** protect) $ break (/=' ') cs
  where hspace_ n = [hspace $ LatexSize $ Em $ 1%2 * fromIntegral n]
protect (c:cs)    = uncurry (++) $ ((:[]) . string . (c :) *** protect) $ break (`elem` " \n") cs

includegraphics = LatexCmd "includegraphics"
tableofcontents = TexCmd "tableofcontents"
vfill = TexCmd "vfill"
hfill = TexCmd "hfill"
textwidth = TexCmd "textwidth"
linewidth = TexCmd "linewidth"
maketitle = TexCmd "maketitle"
newpage = TexCmd "newpage"
par = TexCmd "par"
topsep = TexCmd "topsep"
headheight = TexCmd "headheight"
leftmargin = TexCmd "leftmargin"
rightmargin = TexCmd "rightmargin"
listparindent = TexCmd "listparindent"
parindent = TexCmd "parindent"
itemindent = TexCmd "itemindent"
parsep = TexCmd "parsep"
parskip = TexCmd "parskip"
hline = TexCmd "hline"
noindent = TexCmd "noindent"

-- those could be seen as taking an argument
_Large = TexCmd "Large"
mdseries = TexCmd "mdseries"
ssfamily = TexCmd "ssfamily"
small = TexCmd "small"
huge = TexCmd "huge"
_Huge = TexCmd "Huge"
_HUGE = TexCmd "HUGE"

newline = LatexCmd "newline" mempty
caption = LatexCmd "caption"
label = LatexCmd "label"
ref = LatexCmd "ref"
cite = LatexCmd "cite"
part = LatexCmd "part"
chapter = LatexCmd "chapter"
section = LatexCmd "section"
subsection = LatexCmd "subsection"
subsubsection = LatexCmd "subsubsection"
paragraph = TexGroup . (LatexCmd "paragraph" mempty <>)
bibliography = LatexCmd "bibliography"
bibliographystyle = LatexCmd "bibliographystyle"
hspace = LatexCmd "hspace"
thispagestyle = LatexCmd "thispagestyle"

-- should be \setlength{a}{b}{c} ...
setlength = LatexCmd "setlength"

listLikeEnv name items =
  Environment name [] $ mconcat $ map (TexCmdArg "item" . getLatexItem) items

item :: Latex -> LatexItem
item = LatexItem

itemize :: [LatexItem] -> Latex
itemize = listLikeEnv "itemize"
enumerate :: [LatexItem] -> Latex
enumerate = listLikeEnv "enumerate"

document = Environment "document"
titlepage = Environment "titlepage"
flushleft = Environment "flushleft"
figure = Environment "figure"
boxedminipage = Environment "boxedminipage"

-- tabular ...

-- eqnarraystar = 

a4paper = A4paper

root = Root

book = Book
article = Article
report = Report
letter = Letter

documentclass :: Maybe LatexSize -> Maybe LatexPaper -> DocumentClass -> Preamble
documentclass msize mpaper dc =
  PreambleCmdArgWithOpts "documentclass" (maybeToList (fmap showSize msize) ++
                                          maybeToList (fmap showPaper mpaper))
                                         (RawTex (showDocumentClass dc))

usepackage = PreambleCmdArg "usepackage"
title = PreambleCmdArg "title"
subtitle = PreambleCmdArg "subtitle"
date = PreambleCmdArg "date"
author = PreambleCmdArg "author"
and = TexCmd "and"
authors = author . mconcat . intersperse and
institute = PreambleCmdArg "institute"

-- beamer
-- alert
-- AtBeginSubsection, AtBeginSection
only = LatexCmd "only"
usetheme = PreambleCmdArg "usetheme"
usefontthem = PreambleCmdArg "usefontthem"
