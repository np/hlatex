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

dash1 = RawTex "{-}"
dash2 = RawTex "{--}"
dash3 = RawTex "{---}"

nbsp = RawTex "{~}"
tilde = nbsp

maths = MathsInline
mathsBlock = MathsInline

mstring = RawMaths . concatMap mchar
mint = MathsInt

sub x = MathsConcat [RawMaths "_", MathsGroup x]
sup x = MathsConcat [RawMaths "^", MathsGroup x]
frac x y = MathsCmdArgs "frac" [] [x,y]
sqrt x = MathsCmdArgs "sqrt" [] [x]

sqrt' :: Int -> MathsItem -> MathsItem
sqrt' n x = MathsCmdArgs "sqrt" [show n] [x]

href x y = LatexCmdArgs "href" [x,y]
person name email = href (hstring ("mailto:"++email)) (hstring name)

pt = Pt
-- em = Em
cm = Cm

rule x y = LatexCmdArgs "rule" [x,LatexSize y]

-- simulate the <hr> html tag
hrule = texgroup $ noindent <> rule linewidth (pt 1.5)

normSpaces = unlines . map (unwords . words) . lines

hstring = RawTex . concatMap hchar . concat . intersperse "\n" . filter (not . null) . lines

hchar '\\' = "\backslash"
hchar '~'  = "$\\tilde{}$"
hchar '<'  = "\\textless{}"
hchar '>'  = "\\textgreater{}"
hchar '|'  = "\\textbar{}"
hchar ':'  = "$:$"
hchar '-'  = "{-}" -- to avoid multiple dashes
hchar c | c `elem` "#_&{}$%" = ['\\',c]
        | otherwise          = [c]

mchar '\\' = "\backslash"
mchar '~'  = "\\tilde{}"
mchar ':'  = ":"
mchar '-'  = "{-}" -- to avoid multiple dashes
mchar c | c `elem` "#_&{}$%" = ['\\',c]
        | otherwise          = [c]

protect :: String -> [Latex]
protect ""        = []
protect ('\n':cs) = newline : protect cs
protect (' ':cs)  = uncurry (++) $ (hspace_ . (+1) . length *** protect) $ break (/=' ') cs
  where hspace_ n = [hspace $ LatexSize $ Em $ 1%2 * fromIntegral n]
protect (c:cs)    = uncurry (++) $ ((:[]) . hstring . (c :) *** protect) $ break (`elem` " \n") cs

includegraphics = ParCmdArg "includegraphics"
tableofcontents = TexCmd "tableofcontents" -- TODO
vfill = TexCmd "vfill" -- TODO
hfill = TexCmd "hfill" -- TODO
textwidth = TexCmd "textwidth" -- TODO
linewidth = TexCmd "linewidth" -- TODO
maketitle = ParCmd "maketitle"
newpage = TexCmd "newpage" -- TODO
par = TexCmd "par" -- TODO
topsep = TexCmd "topsep" -- TODO
headheight = TexCmd "headheight" -- TODO
leftmargin = TexCmd "leftmargin" -- TODO
rightmargin = TexCmd "rightmargin" -- TODO
listparindent = TexCmd "listparindent" -- TODO
parindent = TexCmd "parindent" -- TODO
itemindent = TexCmd "itemindent" -- TODO
parsep = TexCmd "parsep" -- TODO
parskip = TexCmd "parskip" -- TODO
hline = TexCmd "hline" -- TODO
noindent = TexCmd "noindent" -- TODO

-- those could be seen as taking an argument
_Large = TexCmd "Large"
mdseries = TexCmd "mdseries"
ssfamily = TexCmd "ssfamily"
small = TexCmd "small"
huge = TexCmd "huge"
_Huge = TexCmd "Huge"
_HUGE = TexCmd "HUGE"

newline = TexCmd "newline"

mbox = LatexCmd "mbox"
footnote = LatexCmd "footnote"
caption = LatexCmd "caption"
label = LatexCmd "label"
ref = LatexCmd "ref"
cite = LatexCmd "cite"

part = ParCmdArg "part"
chapter = ParCmdArg "chapter"
section = ParCmdArg "section"
subsection = ParCmdArg "subsection"
subsubsection = ParCmdArg "subsubsection"
-- paragraph and subparagraph are omitted on purpose

para = Para
bibliography = LatexCmd "bibliography"
bibliographystyle = LatexCmd "bibliographystyle"
hspace = LatexCmd "hspace"
thispagestyle = LatexCmd "thispagestyle"

-- should be \setlength{a}{b}{c} ...
setlength = LatexCmd "setlength"

listLikeEnv name items =
  ParEnvironmentLR name [] $ mconcat $ map (TexCmdArg "item" . getLatexItem) items

item :: Latex -> LatexItem
item = LatexItem

itemize :: [LatexItem] -> ParMode
itemize = listLikeEnv "itemize"
enumerate :: [LatexItem] -> ParMode
enumerate = listLikeEnv "enumerate"
description :: [LatexItem] -> ParMode
description = listLikeEnv "description"

document = Document
titlepage = ParEnvironmentLR "titlepage"
flushleft = ParEnvironmentLR "flushleft"
figure = ParEnvironmentLR "figure"
boxedminipage = ParEnvironmentLR "boxedminipage" -- parmode?
quote = ParEnvironmentLR "quote"
quotation = ParEnvironmentPar "quotation"
verse = ParEnvironmentPar "verse"

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
