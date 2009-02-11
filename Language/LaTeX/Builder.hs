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

  mkTexDecl name =
    let lname = lowerName name in
    [sigD lname [t| Latex |]
    ,valD (varP lname) (normalB [| TexDecl $(stringE name) [] |]) []
    ]

  mkLatexCmd name =
    let lname = lowerName name in
    [sigD lname [t| Latex -> Latex |]
    ,valD (varP lname) (normalB [| LatexCmd $(stringE name) |]) []
    ]

 in sequence $ concat $ concat
      [ map mkMathsCmd mathsCmds
      , map mkMathsCmdArg mathsCmdsArg
      , map mkTexDecl texDecls
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
sqrt' n x = MathsCmdArgs "sqrt" [[show n]] [x]

mleft, mright :: Char -> MathsItem
mleft x = RawMaths ("\\left" ++ [checkParens x])
mright x = RawMaths ("\\right" ++ [checkParens x])

between opening closing x = mleft opening <> x <> mright closing

parens   = between '(' ')'
braces   = between '{' '}'
brackets = between '[' ']'

checkParens x | x `elem` "([{}])" = x
              | otherwise         = error $ "checkParens: invalid parenthesis-like: " ++ show x

href x y = LatexCmdArgs "href" [] [x,y]
person name email = href (hstring ("mailto:"++email)) (hstring name)

pt = Pt
-- em = Em
cm = Cm

rule x y = LatexCmdArgs "rule" [] [x,LatexSize y]

-- simulate the <hr> html tag
hrule = texgroup $ noindent <> rule linewidth (pt 1.5)

normSpaces = unlines . map (unwords . words) . lines

hstring = RawTex . concatMap hchar . concat . intersperse "\n" . filter (not . null) . lines

pstring = para . hstring

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

includegraphics = ParCmdArgs "includegraphics"
tableofcontents = TexCmdNoArg "tableofcontents" -- TODO
vfill = TexCmdNoArg "vfill" -- TODO
hfill = TexCmdNoArg "hfill" -- TODO
textwidth = TexCmdNoArg "textwidth" -- TODO
linewidth = TexCmdNoArg "linewidth" -- TODO
maketitle = ParCmdArgs "maketitle" [] []
newpage = TexCmdNoArg "newpage" -- TODO
par = TexCmdNoArg "par" -- TODO
topsep = TexCmdNoArg "topsep" -- TODO
headheight = TexCmdNoArg "headheight" -- TODO
leftmargin = TexCmdNoArg "leftmargin" -- TODO
rightmargin = TexCmdNoArg "rightmargin" -- TODO
listparindent = TexCmdNoArg "listparindent" -- TODO
parindent = TexCmdNoArg "parindent" -- TODO
itemindent = TexCmdNoArg "itemindent" -- TODO
parsep = TexCmdNoArg "parsep" -- TODO
parskip = TexCmdNoArg "parskip" -- TODO
hline = TexCmdNoArg "hline" -- TODO
noindent = TexCmdNoArg "noindent" -- TODO

-- those could be seen as taking an argument
_Large = TexDecl "Large" []
mdseries = TexDecl "mdseries" []
ssfamily = TexDecl "ssfamily" []
small = TexDecl "small" []
huge = TexDecl "huge" []
_Huge = TexDecl "Huge" []
_HUGE = TexDecl "HUGE" []

newline = TexCmdNoArg "newline"

mbox = LatexCmd "mbox"
footnote = LatexCmd "footnote"
caption = LatexCmd "caption"
label = LatexCmd "label"
ref = LatexCmd "ref"
cite = LatexCmd "cite"

part          = ParCmdArg "part" -- TODO options
chapter       = ParCmdArg "chapter" -- TODO options
section       = ParCmdArg "section" -- TODO options
subsection    = ParCmdArg "subsection" -- TODO options
subsubsection = ParCmdArg "subsubsection" -- TODO options
-- paragraph and subparagraph are omitted on purpose

para = Para
bibliography = LatexCmd "bibliography"
bibliographystyle = LatexCmd "bibliographystyle"
hspace = LatexCmd "hspace"
thispagestyle = LatexCmd "thispagestyle"

-- should be \setlength{a}{b}{c} ...
setlength = LatexCmd "setlength"

listLikeEnv name items =
  ParEnvironmentPar name [] $ mconcat $ map mkItem items
  where mkItem (Item mlabel contents) = ParDecl "item" (maybeToList mlabel) <> contents

item :: ParMode -> Item
item = Item Nothing

item' :: String -> ParMode -> Item
item' = Item . Just

itemize :: [Item] -> ParMode
itemize = listLikeEnv "itemize"
enumerate :: [Item] -> ParMode
enumerate = listLikeEnv "enumerate"
description :: [Item] -> ParMode
description = listLikeEnv "description"

document = Document
titlepage = ParEnvironmentPar "titlepage"
flushleft = ParEnvironmentPar "flushleft"
figure = ParEnvironmentPar "figure"
boxedminipage = ParEnvironmentPar "boxedminipage"
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
and = TexCmdNoArg "and"
authors = author . mconcat . intersperse and
institute = PreambleCmdArg "institute"

-- beamer
-- alert
-- AtBeginSubsection, AtBeginSection
only = LatexCmd "only"
usetheme = PreambleCmdArg "usetheme"
usefontthem = PreambleCmdArg "usefontthem"
