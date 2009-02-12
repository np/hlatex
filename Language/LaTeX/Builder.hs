{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fno-warn-missing-signatures #-}
module Language.LaTeX.Builder where

import Prelude hiding (and)
import Data.List (intersperse)
import Data.Maybe
import Data.Ratio
import Data.Monoid
import Data.Char
import Control.Arrow

import Language.LaTeX.Types
import Language.LaTeX.Data
import Language.LaTeX.Internal
import Language.LaTeX.Printer (ppSize)
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

  mkMathsDecl name =
    let lname = lowerName name in
    [sigD lname [t| MathsItem |]
    ,valD (varP lname) (normalB [| MathsDecl $(stringE name) [] |]) []
    ]

 in sequence $ concat $ concat
      [ map mkMathsCmd mathsCmds
      , map mkMathsCmdArg mathsCmdsArg
      , map mkTexDecl texDecls
      ]
 )

class Group a where
  group :: a -> a

instance Group Latex where group = TexGroup
instance Group MathsItem where group = MathsGroup

dash1 = RawTex "{-}"
dash2 = RawTex "{--}"
dash3 = RawTex "{---}"

nbsp = RawTex "{~}"

maths = MathsInline
displaymath = DisplayMaths

mstring = RawMaths . concatMap mchar'
mint :: Int -> MathsItem
mint = fromIntegral
mrat :: Rational -> MathsItem
mrat = fromRational

sub x = RawMaths "_" <> MathsGroup x
sup x = RawMaths "^" <> MathsGroup x
frac x y = MathsCmdArgs "frac" [] [x,y]
stackrel x y = MathsCmdArgs "stackrel" [] [x,y]

bmod = MathsBinOp "bmod"

sqrt x = MathsCmdArgs "sqrt" [] [x]

sqrt' :: Int -> MathsItem -> MathsItem
sqrt' n x = MathsCmdArgs "sqrt" [[show n]] [x]

eq :: MathsItem
eq = RawMaths "{=}"

mleft, mright :: Char -> MathsItem
mleft x = RawMaths $ "\\left" ++ parenChar x
mright x = RawMaths $ "\\right" ++ parenChar x

between opening closing x = mleft opening <> x <> mright closing

parens   = between '(' ')'
braces   = between '{' '}'
brackets = between '[' ']'

parenChar x | x `elem` "([.])" = [x]
            | x == '{'         = "\\{"
            | x == '}'         = "\\}"
            | otherwise        = error $ "invalid parenthesis-like: " ++ show x

href x y = LatexCmdArgs "href" [(True,x),(True,y)]
person name email = href (hstring ("mailto:"++email)) (hstring name)

pt = Pt
-- em = Em
cm = Cm
mm = Mm
ex = Ex
pc = Pc
inch = In

-- simulate the <hr> html tag
hr = group $ noindent <> rule linewidth (pt 1.5)

normSpaces = unlines . map (unwords . words) . lines

hstring :: String -> Latex
hstring = RawTex . concatMap hchar' . concat . intersperse "\n" . filter (not . null) . lines

pstring = para . hstring

hchar = RawTex . hchar'

hchar' '\\' = "\backslash"
hchar' '~'  = "$\\tilde{}$"
hchar' '<'  = "\\textless{}"
hchar' '>'  = "\\textgreater{}"
hchar' '|'  = "\\textbar{}"
hchar' ':'  = "$:$"
hchar' x | x `elem` "#_&{}$%" = ['\\',x]
         | x `elem` "-]["     = ['{', x, '}'] -- to avoid multiple dashes or mess up optional args
         | otherwise          = [x]

mchar = RawMaths . mchar'

mchar' '\\' = "\backslash"
mchar' '~'  = "\\tilde{}"
mchar' ':'  = ":"
mchar' x | x `elem` "#_&{}$%" = ['\\',x]
         | x `elem` "-]["     = ['{', x, '}'] -- to avoid multiple dashes or mess up optional args
         | otherwise          = [x]

protect :: String -> [Latex]
protect ""        = []
protect ('\n':xs) = newline : protect xs
protect (' ':xs)  = uncurry (++) $ (hspace_ . (+1) . length *** protect) $ break (/=' ') xs
  where hspace_ n = [hspace $ Em $ 1%2 * fromIntegral n]
protect (x:xs)    = uncurry (++) $ ((:[]) . hstring . (x :) *** protect) $ break (`elem` " \n") xs

includegraphics = ParCmdArgs "includegraphics"
tableofcontents = TexCmdNoArg "tableofcontents" -- TODO
maketitle = ParCmdArgs "maketitle" [] []
par = TexCmdNoArg "par" -- TODO
noindent = TexCmdNoArg "noindent" -- TODO

-- robust
stretch = SizeCmdRatArg "stretch"

parindent = SizeCmd "parindent"
textwidth = SizeCmd "textwidth"
linewidth = SizeCmd "linewidth"
textheight = SizeCmd "textheight"
parsep = SizeCmd "parsep"
parskip = SizeCmd "parskip"
baselineskip = SizeCmd "baselineskip"
baselinestrech = SizeCmd "baselinestrech"
fill = SizeCmd "fill"
columnsep = SizeCmd "columnsep"
columnseprule = SizeCmd "columnseprule"
mathindent = SizeCmd "mathindent"
oddsidemargin = SizeCmd "oddsidemargin"
evensidemargin = SizeCmd "evensidemargin"
marginparwidth = SizeCmd "marginparwidth"
marginparsep = SizeCmd "marginparsep"
marginparpush = SizeCmd "marginparpush"
topmargin = SizeCmd "topmargin"
headheight = SizeCmd "headheight"
headsep = SizeCmd "headsep"
topskip = SizeCmd "topskip"
footheight = SizeCmd "footheight"
footskip = SizeCmd "footskip"
topsep = SizeCmd "topsep"
partopsep = SizeCmd "partopsep"
itemsep = SizeCmd "itemsep"
itemindent = SizeCmd "itemindent"
labelsep = SizeCmd "labelsep"
labelwidth = SizeCmd "labelwidth"
leftmargin = SizeCmd "leftmargin"
rightmargin = SizeCmd "rightmargin"
listparindent = SizeCmd "listparindent"
jot = SizeCmd "jot"
abovedisplayskip = SizeCmd "abovedisplayskip"
belowdisplayskip = SizeCmd "belowdisplayskip"
abovedisplayshortskip = SizeCmd "abovedisplayshortskip"
belowdisplayshortskip = SizeCmd "belowdisplayshortskip"
floatsep = SizeCmd "floatsep"
textfloatsep = SizeCmd "textfloatsep"
intextsep = SizeCmd "intextsep"
dblfloatsep = SizeCmd "dblfloatsep"
dbltextfloatsep = SizeCmd "dbltextfloatsep"
textfraction = SizeCmd "textfraction"
floatpagefraction = SizeCmd "floatpagefraction"
dbltopfaction = SizeCmd "dbltopfaction"
dblfloatpagefraction = SizeCmd "dblfloatpagefraction"
arraycolsep = SizeCmd "arraycolsep"
tabcolsep = SizeCmd "tabcolsep"
arrayrulewidth = SizeCmd "arrayrulewidth"
doublerulesep = SizeCmd "doublerulesep"
arraystretch = SizeCmd "arraystretch"
bigskipamount = SizeCmd "bigskipamount"
medskipamount = SizeCmd "medskipamount"
smallskipamount = SizeCmd "smallskipamount"
fboxrule = SizeCmd "fboxrule"
fboxsep = SizeCmd "fboxsep"

-- Marginal Notes

reversemarginpar = TexDecl "reversemarginpar"
normalmarginpar = TexDecl "normalmarginpar"

-- The tabbing Environment

-- TODO

-- Spaces

-- robust
hspace = LatexCmd "hspace" . LatexSize

-- robust
hspaceStar = LatexCmd "hspace*" . LatexSize

-- fragile
vspace = LatexCmd "vspace" . LatexSize

-- fragile
vspaceStar = LatexCmd "vspace*" . LatexSize

vfill = TexCmdNoArg "vfill" -- = vspace fill
hfill = TexCmdNoArg "hfill" -- = hspace fill
dotfill = TexCmdNoArg "dotfill"
hrulefill = TexCmdNoArg "hrulefill"

-- fragile
bigskip = TexCmdNoArg "bigskip" -- = vspace bigskipamount

-- fragile
medskip = TexCmdNoArg "medskip" -- = vspace medskipamount

-- fragile
smallskip = TexCmdNoArg "smallskip" -- = vspace smallskipamount

addvspace = LatexCmd "addvspace" . LatexSize


-- Font sizes

-- those could be seen as taking an argument
tiny         = TexDecl "tiny" []
scriptsize   = TexDecl "scriptsize" []
footnotesize = TexDecl "footnotesize" []
small        = TexDecl "small" []
normalsize   = TexDecl "normalsize" []
large        = TexDecl "large" []
_LARGE       = TexDecl "LARGE" []
_Large       = TexDecl "Large" []
huge         = TexDecl "huge" []
_Huge        = TexDecl "Huge" []

mdseries = TexDecl "mdseries" []
ssfamily = TexDecl "ssfamily" []

-- textXYZ commands should work in maths too (use a typeclass)
emph = LatexCmd "emph"
textrm = LatexCmd "textrm"
textsf = LatexCmd "textsf"
texttt = LatexCmd "texttt"
textmd = LatexCmd "textmd"
textbf = LatexCmd "textbf"
textup = LatexCmd "textup"
textit = LatexCmd "textit"
textsl = LatexCmd "textsl"
textsc = LatexCmd "textsc"
textnormal = LatexCmd "textnormal"

-- Line and page breaking

-- fragile
linebreak, nolinebreak :: Int -> Latex
linebreak = TexDecl "linebreak" . (:[]) . show
nolinebreak = TexDecl "nolinebreak" . (:[]) . show

-- fragile
newline = TexCmdNoArg "newline"
-- newline' x = TexDecl "newline" [LatexSize x]

-- robust
hyphen = RawTex "{\\-}" -- check if {...} does not cause trouble here

-- robust
hyphenation :: [String] -> ParMode
hyphenation = ParCmdArgs "hyphenation" [] . (:[]) . RawTex . unwords -- RawTex is a bit rough here

sloppy = TexDecl "sloppy" []
fussy = TexDecl "fussy" []

sloppypar = ParEnvironmentPar "sloppypar" []

-- fragile
pagebreak, nopagebreak :: Int -> Latex
pagebreak = TexDecl "pagebreak" . (:[]) . show
nopagebreak = TexDecl "nopagebreak" . (:[]) . show

-- fragile
samepage = TexDecl "samepage" []

-- robust
newpage = ParDecl "newpage" []
-- robust
clearpage = ParDecl "clearpage" []
-- fragile
cleardoublepage = ParDecl "cleardoublepage" []

--- Boxes

{-
class Mbox a where
  -- robust
  mbox :: Latex -> a

  -- fragile
  makebox :: LatexSize -> LatexSize -> Latex -> a

instance Mbox MathsItem where
  mbox = MathsToLR Nothing Nothing
  makebox = mmakebox

instance Mbox Latex where
  mbox = id

instance Mbox ParMode where
-}

text = MathsNeedsPackage "amsmath" . MathsToLR "text"

-- robust
mbox = LatexCmd "mbox"

-- fragile
makebox width txt = LatexCmdArgs "makebox" [(False,LatexSize width),(True,txt)]

-- fragile
makeboxLeft width txt =
  LatexCmdArgs "makebox" [(False,LatexSize width),(False,RawTex "l"),(True,txt)]

-- fragile
makeboxRight width txt =
  LatexCmdArgs "makebox" [(False,LatexSize width),(False,RawTex "r"),(True,txt)]

-- robust
fbox = LatexCmd "fbox"

-- fragile
framebox width txt = LatexCmdArgs "framebox" [(False,LatexSize width),(True,txt)]

-- fragile
frameboxLeft width txt =
  LatexCmdArgs "framebox" [(False,LatexSize width),(False,RawTex "l"),(True,txt)]

-- fragile
frameboxRight width txt =
  LatexCmdArgs "framebox" [(False,LatexSize width),(False,RawTex "r"),(True,txt)]

-- TODO: make a safe version using a monad
-- fragile
unsafeNewsavebox i =
  let bin = UnsafeMakeSaveBin i
  in LatexCmd "newsavebox" $ LatexSaveBin bin

-- robust
sbox bin txt = LatexCmdArgs "sbox" [(True,LatexSaveBin bin), (True,txt)]

-- fragile
savebox bin width txt =
  LatexCmdArgs "savebox" [(True, LatexSaveBin bin), (False, LatexSize width),
                          (True, txt)]

-- fragile
saveboxLeft bin width txt =
  LatexCmdArgs "savebox" [(True, LatexSaveBin bin), (False, LatexSize width),
                          (False, RawTex "l"), (True, txt)]

-- fragile
saveboxRight bin width txt =
  LatexCmdArgs "savebox" [(True, LatexSaveBin bin), (False, LatexSize width),
                          (False, RawTex "r"), (True, txt)]

-- robust
usebox bin = LatexCmdArgs "usebox" [(True, LatexSaveBin bin)]

-- fragile
parbox width txt =
  LatexCmdArgs "parbox" [(True, LatexSize width), (True, txt)]

-- fragile
parboxTop width txt =
  LatexCmdArgs "parbox" [(False, RawTex "t"), (True, LatexSize width), (True, txt)]

-- fragile
parboxBot width txt =
  LatexCmdArgs "parbox" [(False, RawTex "b"), (True, LatexSize width), (True, txt)]

minipage width txt =
  LatexCmdArgs "minipage" [(True, LatexSize width), (True, LatexParMode txt)]

minipageTop width txt =
  LatexCmdArgs "minipage" [(False, RawTex "t"), (True, LatexSize width), (True, txt)]

minipageBot width txt =
  LatexCmdArgs "minipage" [(False, RawTex "b"), (True, LatexSize width), (True, txt)]

-- fragile
rule width height = LatexCmdArgs "rule" [(True,LatexSize width),(True,LatexSize height)]

-- fragile
rule' raise_len width height = LatexCmdArgs "rule" [(False, LatexSize raise_len)
                                                   ,(True,LatexSize width),(True,LatexSize height)]

-- fragile
raisebox raise_len txt =
  LatexCmdArgs "raisebox" [(True,LatexSize raise_len),(True,txt)]

-- fragile
raisebox' raise_len height depth txt =
  LatexCmdArgs "raisebox" [(True,LatexSize raise_len)
                          ,(False,LatexSize height),(False,LatexSize depth),(True, txt)]

footnote = LatexCmd "footnote"

caption :: Latex -> Latex
caption txt = LatexCmdArgs "caption" [(True,txt)]
caption' :: String -> Latex -> Latex
caption' lstentry txt = LatexCmdArgs "caption" [(False,checkentry lstentry), (True,txt)]
  where checkentry x
          | all isAlphaNum x = RawTex x
          | otherwise        = error "caption': restriction to alphanumeric characters for the lstentry"

label = LatexCmd "label" . LatexKeys . (:[])
ref = LatexCmd "ref" . LatexKeys . (:[])
pageref = LatexCmd "pageref" . LatexKeys . (:[])

-- fragile
cite = LatexCmd "cite" . LatexKeys
cite' txt keys = LatexCmdArgs "cite" [(False,txt), (True,LatexKeys keys)]

-- fragile
nocite = LatexCmd "nocite" . LatexKeys

part          = ParCmdArg "part" -- TODO options
chapter       = ParCmdArg "chapter" -- TODO options
section       = ParCmdArg "section" -- TODO options
subsection    = ParCmdArg "subsection" -- TODO options
subsubsection = ParCmdArg "subsubsection" -- TODO options
-- paragraph and subparagraph are omitted on purpose

para = Para
bibliography = LatexCmd "bibliography"
bibliographystyle = LatexCmd "bibliographystyle"
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
figure = FigureLike "figure"
figureStar = FigureLike "figure*"
table = FigureLike "table"
tableStar = FigureLike "table*"
boxedminipage = ParEnvironmentPar "boxedminipage"
quote = ParEnvironmentLR "quote"
quotation = ParEnvironmentPar "quotation"
verse = ParEnvironmentPar "verse"

-- The array and tablular Environments

-- use a monad to report errors
tabular specs rows = Tabular specs $ checkRows specs rows

array specs rows = MathsArray specs $ checkRows specs rows

checkRows specs = map checkRow
  where checkRow (Cells cs)
          | cols /= length cs    = err "wrong number of cells" cols "different from" (length cs)
          | otherwise            = Cells cs
        checkRow Hline           = Hline
        checkRow (Cline c1 c2)
          | c1 > cols = err "cline: start column too high" c1 ">" cols
          | c1 < 0    = error "tabular: cline: negative start column"
          | c2 > cols = err "cline: end column too high" c2 ">" cols
          | c2 < 0    = error "tabular: cline: negative end column"
          | otherwise = Cline c1 c2
        cols = length $ filter isCol specs
        isCol s = s `elem` [Rc,Rl,Rr]
        err msg x op y = error $ unwords ["tabular:", msg, "(" ++ show x, op, show y ++ ")"] 

cells = Cells
hline = Hline
cline = Cline

-- this is more the '|' than the \vline of LaTeX,
-- one may want to support both using a HaveVline type class.
vline = Rvline

class HaveC a where c :: a
class HaveL a where l :: a
class HaveR a where r :: a

instance HaveC RowSpec where c = Rc
instance HaveL RowSpec where l = Rl
instance HaveR RowSpec where r = Rr

-- eqnarraystar = 

a4paper = A4paper

root = Root

book = Book
article = Article
report = Report
letter = Letter

documentclass :: Maybe LatexSize -> Maybe LatexPaper -> DocumentClass -> Preamble
documentclass msize mpaper dc =
  PreambleCmdArgWithOpts "documentclass" (maybeToList (fmap (($"") . ppSize) msize) ++
                                          maybeToList (fmap showPaper mpaper))
                                         (RawTex $ showDocumentClass dc)

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
