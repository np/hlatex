{-# OPTIONS -fno-warn-missing-signatures #-}
module Language.LaTeX.Builder where

import Prelude ( (.), ($), (<), (>), Eq(..), Num(..), Functor(..), error, show, otherwise
               , Bool(..), not, fromIntegral, fromRational, Int, uncurry)
import Data.List hiding (sum, and, group) -- (map, (++), elem, intersperse, length, unwords, filter, break, all,)
import Data.Maybe
import Data.Ratio
import Data.Monoid
import Data.Char
import Control.Arrow

import Language.LaTeX.Types
import Language.LaTeX.Internal
import Language.LaTeX.Printer (ppSize)

{-
import Prelude (writeFile, id, sequence, Monad(..), fst)
import Language.Haskell.TH
-}

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

{-
$(
 let
  mathsCmdsArg, texDecls, mathsDecls :: [String]
  mathsCmds :: [(String, String)]

  mathsCmds =
    [("lbrace", "{")
    ,("rbrace", "}")
    ,("space", " ")
    ,("at", "@")
    ,("in_", "in")
    ,("forall_", "forall")
    ,("mthinspace", ",")
    ,("mnegthinspace", "!")
    ,("mmediumspace", ":")
    ,("mthickspace", ";")
    ,("msup", "sup")
    ] ++ map (id &&& id)
    [-- Greek letters
    "alpha","beta","chi","delta","Delta","epsilon","varepsilon","eta","gamma"
    ,"Gamma","iota","kappa","lambda","Lambda","mu","nu","omega","Omega","phi"
    ,"varphi","Phi","pi","Pi","psi","rho","sigma","Sigma","tau","theta"
    ,"vartheta","Theta","upsilon","xi","Xi","zeta"

    -- Operation symbols
    ,"backslash","times","divide","circ","oplus","otimes","sum","prod","wedge"
    ,"bigwedge","vee","bigvee","cup","bigcup","cap","bigcap"

    -- Relation symbols
    ,"ne","le","leq","ge","geq","prec","succ","notin","subset","supset"
    ,"subseteq","supseteq","equiv","cong","approx","propto"

    -- Logical symbols
    ,"neg","implies","iff","exists","bot","top","vdash","models"

    -- Grouping brackets
    ,"langle","rangle"

    -- Miscellaneous symbols
    ,"int","oint","partial","nabla","pm","emptyset","infty","aleph","ldots"
    ,"cdots","vdots","ddots","quad","diamond","square","lfloor","rfloor","lceiling","rceiling"

    -- Standard functions
    ,"sin","cos","tan","csc","sec","cot","sinh","cosh","tanh","log"
    ,"ln","det","dim","lim","mod","gcd","lcm","liminf","inf","limsup"
    ,"max","min","Pr"

    -- Arrows
    ,"uparrow","downarrow","rightarrow","to","leftarrow"
    ,"leftrightarrow","Rightarrow","Leftarrow","Leftrightarrow"
    ]

  mathsCmdsArg =
    [-- Font commands
    "mathbf","mathbb","mathcal","mathtt","mathfrak"
    -------
    ,"pmod"
    -- Putting one thing above another
    ,"tilde", "hat", "check", "breve", "acute", "grave", "bar", "vec"
    , "dot", "ddot", "overbrace", "underbrace"
    ,"overline","underline","widehat","widetilde","imath","jmath"
    ]

  typeStyles :: [String]
  typeStyles = ["em","bf","sf","sl","sc","it","tt"]

  texDecls = typeStyles

  mathsDecls = ["displaystyle", "textstyle", "scriptstyle", "scriptscriptstyle"
              ,"mit","cal"
              ]

  lowerName :: String -> Name
  lowerName name | isLower (head name) = mkName name
                | otherwise           = mkName $ '_':name

  mkMathsCmd (name, _) =
    let lname = lowerName name
    in
    [sigD lname [t| MathsItem |]
    , valD (varP lname) (normalB [| MathsCmd $(stringE name) |]) []
    ]

  mkMathsCmdArg name =
    let lname = lowerName name in
    [sigD lname [t| MathsItem -> MathsItem |]
    , valD (varP lname) (normalB [| MathsCmdArg $(stringE name) |]) []
    ]

  mkTexDecl name =
    let lname = lowerName name in
    [sigD lname [t| Latex |]
    , valD (varP lname) (normalB [| TexDecl $(stringE name) [] |]) []
    ]

  mkMathsDecl name =
    let lname = lowerName name in
    [sigD lname [t| MathsItem |]
     , valD (varP lname) (normalB [| MathsDecl $(stringE name) [] |]) []
    ]

  mkList name ty names =
    [sigD name ty
    ,valD (varP name) (normalB (listE $ map (varE . lowerName) names)) []]

  d = sequence $ concat $ concat
      [ map mkMathsCmd mathsCmds
      , map mkMathsCmdArg mathsCmdsArg
      , map mkTexDecl texDecls
      , map mkMathsDecl mathsDecls
      , [mkList (mkName "mathsCmds") [t| [MathsItem] |] $ map fst mathsCmds]
      , [mkList (mkName "mathsCmdsArg") [t| [MathsItem -> MathsItem] |] mathsCmdsArg]
      , [mkList (mkName "mathsDecls") [t| [MathsItem] |] mathsDecls]
      , [mkList (mkName "texDecls") [t| [Latex] |] texDecls]
      ]
  in do dd <- d
        runIO $ writeFile "/tmp/a.hs" $ pprint dd
        return []
 )
 -}

{- This chunk was generated by the previous TH splice. -}
usepackage = PreambleCmdArg "usepackage"
title = PreambleCmdArg "title"
subtitle = PreambleCmdArg "subtitle"
date = PreambleCmdArg "date"
author = PreambleCmdArg "author"
and = TexCmdNoArg "and"
authors = author . mconcat . intersperse and
institute = PreambleCmdArg "institute"


lbrace :: MathsItem
lbrace = MathsCmd "lbrace"
rbrace :: MathsItem
rbrace = MathsCmd "rbrace"
space :: MathsItem
space = MathsCmd "space"
at :: MathsItem
at = MathsCmd "at"
in_ :: MathsItem
in_ = MathsCmd "in_"
forall_ :: MathsItem
forall_ = MathsCmd "forall_"
mthinspace :: MathsItem
mthinspace = MathsCmd "mthinspace"
mnegthinspace :: MathsItem
mnegthinspace = MathsCmd "mnegthinspace"
mmediumspace :: MathsItem
mmediumspace = MathsCmd "mmediumspace"
mthickspace :: MathsItem
mthickspace = MathsCmd "mthickspace"
msup :: MathsItem
msup = MathsCmd "msup"
alpha :: MathsItem
alpha = MathsCmd "alpha"
beta :: MathsItem
beta = MathsCmd "beta"
chi :: MathsItem
chi = MathsCmd "chi"
delta :: MathsItem
delta = MathsCmd "delta"
_Delta :: MathsItem
_Delta = MathsCmd "Delta"
epsilon :: MathsItem
epsilon = MathsCmd "epsilon"
varepsilon :: MathsItem
varepsilon = MathsCmd "varepsilon"
eta :: MathsItem
eta = MathsCmd "eta"
gamma :: MathsItem
gamma = MathsCmd "gamma"
_Gamma :: MathsItem
_Gamma = MathsCmd "Gamma"
iota :: MathsItem
iota = MathsCmd "iota"
kappa :: MathsItem
kappa = MathsCmd "kappa"
lambda :: MathsItem
lambda = MathsCmd "lambda"
_Lambda :: MathsItem
_Lambda = MathsCmd "Lambda"
mu :: MathsItem
mu = MathsCmd "mu"
nu :: MathsItem
nu = MathsCmd "nu"
omega :: MathsItem
omega = MathsCmd "omega"
_Omega :: MathsItem
_Omega = MathsCmd "Omega"
phi :: MathsItem
phi = MathsCmd "phi"
varphi :: MathsItem
varphi = MathsCmd "varphi"
_Phi :: MathsItem
_Phi = MathsCmd "Phi"
pi :: MathsItem
pi = MathsCmd "pi"
_Pi :: MathsItem
_Pi = MathsCmd "Pi"
psi :: MathsItem
psi = MathsCmd "psi"
rho :: MathsItem
rho = MathsCmd "rho"
sigma :: MathsItem
sigma = MathsCmd "sigma"
_Sigma :: MathsItem
_Sigma = MathsCmd "Sigma"
tau :: MathsItem
tau = MathsCmd "tau"
theta :: MathsItem
theta = MathsCmd "theta"
vartheta :: MathsItem
vartheta = MathsCmd "vartheta"
_Theta :: MathsItem
_Theta = MathsCmd "Theta"
upsilon :: MathsItem
upsilon = MathsCmd "upsilon"
xi :: MathsItem
xi = MathsCmd "xi"
_Xi :: MathsItem
_Xi = MathsCmd "Xi"
zeta :: MathsItem
zeta = MathsCmd "zeta"
backslash :: MathsItem
backslash = MathsCmd "backslash"
times :: MathsItem
times = MathsCmd "times"
divide :: MathsItem
divide = MathsCmd "divide"
circ :: MathsItem
circ = MathsCmd "circ"
oplus :: MathsItem
oplus = MathsCmd "oplus"
otimes :: MathsItem
otimes = MathsCmd "otimes"
sum :: MathsItem
sum = MathsCmd "sum"
prod :: MathsItem
prod = MathsCmd "prod"
wedge :: MathsItem
wedge = MathsCmd "wedge"
bigwedge :: MathsItem
bigwedge = MathsCmd "bigwedge"
vee :: MathsItem
vee = MathsCmd "vee"
bigvee :: MathsItem
bigvee = MathsCmd "bigvee"
cup :: MathsItem
cup = MathsCmd "cup"
bigcup :: MathsItem
bigcup = MathsCmd "bigcup"
cap :: MathsItem
cap = MathsCmd "cap"
bigcap :: MathsItem
bigcap = MathsCmd "bigcap"
ne :: MathsItem
ne = MathsCmd "ne"
le :: MathsItem
le = MathsCmd "le"
leq :: MathsItem
leq = MathsCmd "leq"
ge :: MathsItem
ge = MathsCmd "ge"
geq :: MathsItem
geq = MathsCmd "geq"
prec :: MathsItem
prec = MathsCmd "prec"
succ :: MathsItem
succ = MathsCmd "succ"
notin :: MathsItem
notin = MathsCmd "notin"
subset :: MathsItem
subset = MathsCmd "subset"
supset :: MathsItem
supset = MathsCmd "supset"
subseteq :: MathsItem
subseteq = MathsCmd "subseteq"
supseteq :: MathsItem
supseteq = MathsCmd "supseteq"
equiv :: MathsItem
equiv = MathsCmd "equiv"
cong :: MathsItem
cong = MathsCmd "cong"
approx :: MathsItem
approx = MathsCmd "approx"
propto :: MathsItem
propto = MathsCmd "propto"
neg :: MathsItem
neg = MathsCmd "neg"
implies :: MathsItem
implies = MathsCmd "implies"
iff :: MathsItem
iff = MathsCmd "iff"
exists :: MathsItem
exists = MathsCmd "exists"
bot :: MathsItem
bot = MathsCmd "bot"
top :: MathsItem
top = MathsCmd "top"
vdash :: MathsItem
vdash = MathsCmd "vdash"
models :: MathsItem
models = MathsCmd "models"
langle :: MathsItem
langle = MathsCmd "langle"
rangle :: MathsItem
rangle = MathsCmd "rangle"
int :: MathsItem
int = MathsCmd "int"
oint :: MathsItem
oint = MathsCmd "oint"
partial :: MathsItem
partial = MathsCmd "partial"
nabla :: MathsItem
nabla = MathsCmd "nabla"
pm :: MathsItem
pm = MathsCmd "pm"
emptyset :: MathsItem
emptyset = MathsCmd "emptyset"
infty :: MathsItem
infty = MathsCmd "infty"
aleph :: MathsItem
aleph = MathsCmd "aleph"
ldots :: MathsItem
ldots = MathsCmd "ldots"
cdots :: MathsItem
cdots = MathsCmd "cdots"
vdots :: MathsItem
vdots = MathsCmd "vdots"
ddots :: MathsItem
ddots = MathsCmd "ddots"
quad :: MathsItem
quad = MathsCmd "quad"
diamond :: MathsItem
diamond = MathsCmd "diamond"
square :: MathsItem
square = MathsCmd "square"
lfloor :: MathsItem
lfloor = MathsCmd "lfloor"
rfloor :: MathsItem
rfloor = MathsCmd "rfloor"
lceiling :: MathsItem
lceiling = MathsCmd "lceiling"
rceiling :: MathsItem
rceiling = MathsCmd "rceiling"
sin :: MathsItem
sin = MathsCmd "sin"
cos :: MathsItem
cos = MathsCmd "cos"
tan :: MathsItem
tan = MathsCmd "tan"
csc :: MathsItem
csc = MathsCmd "csc"
sec :: MathsItem
sec = MathsCmd "sec"
cot :: MathsItem
cot = MathsCmd "cot"
sinh :: MathsItem
sinh = MathsCmd "sinh"
cosh :: MathsItem
cosh = MathsCmd "cosh"
tanh :: MathsItem
tanh = MathsCmd "tanh"
log :: MathsItem
log = MathsCmd "log"
ln :: MathsItem
ln = MathsCmd "ln"
det :: MathsItem
det = MathsCmd "det"
dim :: MathsItem
dim = MathsCmd "dim"
lim :: MathsItem
lim = MathsCmd "lim"
mod :: MathsItem
mod = MathsCmd "mod"
gcd :: MathsItem
gcd = MathsCmd "gcd"
lcm :: MathsItem
lcm = MathsCmd "lcm"
liminf :: MathsItem
liminf = MathsCmd "liminf"
inf :: MathsItem
inf = MathsCmd "inf"
limsup :: MathsItem
limsup = MathsCmd "limsup"
max :: MathsItem
max = MathsCmd "max"
min :: MathsItem
min = MathsCmd "min"
_Pr :: MathsItem
_Pr = MathsCmd "Pr"
uparrow :: MathsItem
uparrow = MathsCmd "uparrow"
downarrow :: MathsItem
downarrow = MathsCmd "downarrow"
rightarrow :: MathsItem
rightarrow = MathsCmd "rightarrow"
to :: MathsItem
to = MathsCmd "to"
leftarrow :: MathsItem
leftarrow = MathsCmd "leftarrow"
leftrightarrow :: MathsItem
leftrightarrow = MathsCmd "leftrightarrow"
_Rightarrow :: MathsItem
_Rightarrow = MathsCmd "Rightarrow"
_Leftarrow :: MathsItem
_Leftarrow = MathsCmd "Leftarrow"
_Leftrightarrow :: MathsItem
_Leftrightarrow = MathsCmd "Leftrightarrow"
mathbf :: MathsItem -> MathsItem
mathbf = MathsCmdArg "mathbf"
mathbb :: MathsItem -> MathsItem
mathbb = MathsCmdArg "mathbb"
mathcal :: MathsItem -> MathsItem
mathcal = MathsCmdArg "mathcal"
mathtt :: MathsItem -> MathsItem
mathtt = MathsCmdArg "mathtt"
mathfrak :: MathsItem -> MathsItem
mathfrak = MathsCmdArg "mathfrak"
pmod :: MathsItem -> MathsItem
pmod = MathsCmdArg "pmod"
tilde :: MathsItem -> MathsItem
tilde = MathsCmdArg "tilde"
hat :: MathsItem -> MathsItem
hat = MathsCmdArg "hat"
check :: MathsItem -> MathsItem
check = MathsCmdArg "check"
breve :: MathsItem -> MathsItem
breve = MathsCmdArg "breve"
acute :: MathsItem -> MathsItem
acute = MathsCmdArg "acute"
grave :: MathsItem -> MathsItem
grave = MathsCmdArg "grave"
bar :: MathsItem -> MathsItem
bar = MathsCmdArg "bar"
vec :: MathsItem -> MathsItem
vec = MathsCmdArg "vec"
dot :: MathsItem -> MathsItem
dot = MathsCmdArg "dot"
ddot :: MathsItem -> MathsItem
ddot = MathsCmdArg "ddot"
overbrace :: MathsItem -> MathsItem
overbrace = MathsCmdArg "overbrace"
underbrace :: MathsItem -> MathsItem
underbrace = MathsCmdArg "underbrace"
overline :: MathsItem -> MathsItem
overline = MathsCmdArg "overline"
underline :: MathsItem -> MathsItem
underline = MathsCmdArg "underline"
widehat :: MathsItem -> MathsItem
widehat = MathsCmdArg "widehat"
widetilde :: MathsItem -> MathsItem
widetilde = MathsCmdArg "widetilde"
imath :: MathsItem -> MathsItem
imath = MathsCmdArg "imath"
jmath :: MathsItem -> MathsItem
jmath = MathsCmdArg "jmath"
em :: Latex
em = TexDecl "em" []
bf :: Latex
bf = TexDecl "bf" []
sf :: Latex
sf = TexDecl "sf" []
sl :: Latex
sl = TexDecl "sl" []
sc :: Latex
sc = TexDecl "sc" []
it :: Latex
it = TexDecl "it" []
tt :: Latex
tt = TexDecl "tt" []
displaystyle :: MathsItem
displaystyle = MathsDecl "displaystyle" []
textstyle :: MathsItem
textstyle = MathsDecl "textstyle" []
scriptstyle :: MathsItem
scriptstyle = MathsDecl "scriptstyle" []
scriptscriptstyle :: MathsItem
scriptscriptstyle = MathsDecl "scriptscriptstyle" []
mit :: MathsItem
mit = MathsDecl "mit" []
cal :: MathsItem
cal = MathsDecl "cal" []
mathsCmds :: [MathsItem]
mathsCmds = [lbrace, rbrace, space, at, in_, forall_, mthinspace, mnegthinspace, mmediumspace,
             mthickspace, msup, alpha, beta, chi, delta, _Delta, epsilon, varepsilon, eta,
             gamma, _Gamma, iota, kappa, lambda, _Lambda, mu, nu, omega, _Omega, phi, varphi,
             _Phi, pi, _Pi, psi, rho, sigma, _Sigma, tau, theta, vartheta, _Theta, upsilon,
             xi, _Xi, zeta, backslash, times, divide, circ, oplus, otimes, sum, prod, wedge,
             bigwedge, vee, bigvee, cup, bigcup, cap, bigcap, ne, le, leq, ge, geq, prec, succ,
             notin, subset, supset, subseteq, supseteq, equiv, cong, approx, propto, neg, implies,
             iff, exists, bot, top, vdash, models, langle, rangle, int, oint, partial, nabla, pm,
             emptyset, infty, aleph, ldots, cdots, vdots, ddots, quad, diamond, square, lfloor,
             rfloor, lceiling, rceiling, sin, cos, tan, csc, sec, cot, sinh, cosh, tanh, log, ln,
             det, dim, lim, mod, gcd, lcm, liminf, inf, limsup, max, min, _Pr, uparrow, downarrow,
             rightarrow, to, leftarrow, leftrightarrow, _Rightarrow, _Leftarrow, _Leftrightarrow]
mathsCmdsArg :: [MathsItem -> MathsItem]
mathsCmdsArg = [mathbf, mathbb, mathcal, mathtt, mathfrak, pmod, tilde, hat, check,
                breve, acute, grave, bar, vec, dot, ddot, overbrace, underbrace, overline,
                underline, widehat, widetilde, imath, jmath]
mathsDecls :: [MathsItem]
mathsDecls = [displaystyle, textstyle, scriptstyle, scriptscriptstyle, mit, cal]
texDecls :: [Latex]
texDecls = [em, bf, sf, sl, sc, it, tt]

-- beamer
-- alert
-- AtBeginSubsection, AtBeginSection
only = LatexCmd "only"
usetheme = PreambleCmdArg "usetheme"
usefontthem = PreambleCmdArg "usefontthem"
