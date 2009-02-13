{-# OPTIONS -fno-warn-missing-signatures #-}
module Language.LaTeX.Builder where

import Prelude ( (.), ($), (<), (>), (>=), (<=), Eq(..), Num(..), Functor(..), (&&)
               , error, show, otherwise, toInteger
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
instance Group MathItem where group = MathGroup

mandatory, optional :: a -> Arg a
mandatory x = Arg Mandatory x
optional x = Arg Optional x

mathCmdArg :: String -> MathItem -> MathItem
mathCmdArg x y = MathCmdArgs x [mandatory y]

parCmdArg :: String -> Latex -> ParMode
parCmdArg x y = ParCmdArgs x [mandatory y]

latexCmdArg :: String -> Latex -> Latex
latexCmdArg x y = LatexCmdArgs x [mandatory y]

mathCmd :: String -> MathItem
mathCmd x = MathCmdArgs x []

dash1 = RawTex "{-}"
dash2 = RawTex "{--}"
dash3 = RawTex "{---}"

nbsp = RawTex "{~}"

math = MathInline
displaymath = DisplayMath

mstring = RawMath . concatMap mchar'
mint :: Int -> MathItem
mint = fromIntegral
mrat :: Rational -> MathItem
mrat = fromRational

sub x = RawMath "_" <> MathGroup x
sup x = RawMath "^" <> MathGroup x
frac x y = MathCmdArgs "frac" [mandatory x,mandatory y]
stackrel x y = MathCmdArgs "stackrel" [mandatory x,mandatory y]

sqrt :: MathItem -> MathItem
sqrt x = MathCmdArgs "sqrt" [mandatory x]

sqrt' :: MathItem -> MathItem -> MathItem
sqrt' n x = MathCmdArgs "sqrt" [optional n, mandatory x]

mleft, mright :: Char -> MathItem
mleft x = RawMath $ "\\left" ++ parenChar x
mright x = RawMath $ "\\right" ++ parenChar x

between opening closing x = mleft opening <> x <> mright closing

parens   = between '(' ')'
braces   = between '{' '}'
brackets = between '[' ']'

parenChar x | x `elem` "([.])" = [x]
            | x == '{'         = "\\{"
            | x == '}'         = "\\}"
            | otherwise        = error $ "invalid parenthesis-like: " ++ show x

href x y = LatexCmdArgs "href" [mandatory x,mandatory y]
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

hint :: Int -> Latex
hint = LatexSize . SizeInt . toInteger

hstring :: String -> Latex
hstring = RawTex . concatMap hchar' . concat . intersperse "\n" . filter (not . null) . lines

pstring = para . hstring

hchar = RawTex . hchar'

hchar' '\\' = "\\textbackslash{}"
hchar' '~'  = "\\~{}"
hchar' '<'  = "\\textless{}"
hchar' '>'  = "\\textgreater{}"
hchar' '^'  = "\\^{}"
hchar' '|'  = "\\textbar{}"
hchar' ':'  = "$:$" -- or maybe "{:}"
hchar' x | x `elem` "#_&{}$%" = ['\\',x]
         | x `elem` "-]["     = ['{', x, '}'] -- to avoid multiple dashes or mess up optional args
         | otherwise          = [x]

mchar = RawMath . mchar'

mchar' '\\' = "\\textbackslash{}"
mchar' '~'  = "\\text{\\~{}}"
mchar' '^'  = "\\^{}"
mchar' ':'  = ":"
mchar' x | x `elem` "#_&{}$%" = ['\\',x]
         | x `elem` "-]["     = ['{', x, '}'] -- to avoid multiple dashes or mess up optional args
         | otherwise          = [x]

protect :: String -> Latex
protect ""        = mempty
protect ('\n':xs) = newline <> protect xs
protect (' ':xs)  = uncurry (<>) $ (hspace_ . (+1) . length *** protect) $ break (/=' ') xs
  where hspace_ n = hspace $ Em $ 1%2 * fromIntegral n
protect (x:xs)    = uncurry (<>) $ (hstring . (x :) *** protect) $ break (`elem` " \n") xs

includegraphics = ParCmdArgs "includegraphics"
tableofcontents = ParDecl "tableofcontents"
maketitle = ParCmdArgs "maketitle" []
-- par = TexCmdNoArg "par"
noindent = TexCmdNoArg "noindent"

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
hspace = latexCmdArg "hspace" . LatexSize

-- robust
hspaceStar = latexCmdArg "hspace*" . LatexSize

-- fragile
vspace = latexCmdArg "vspace" . LatexSize

-- fragile
vspaceStar = latexCmdArg "vspace*" . LatexSize

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

addvspace = latexCmdArg "addvspace" . LatexSize


-- Font sizes

-- those could be seen as taking an argument
tiny         = TexDecl "tiny"
scriptsize   = TexDecl "scriptsize"
footnotesize = TexDecl "footnotesize"
small        = TexDecl "small"
normalsize   = TexDecl "normalsize"
large        = TexDecl "large"
_LARGE       = TexDecl "LARGE"
_Large       = TexDecl "Large"
huge         = TexDecl "huge"
_Huge        = TexDecl "Huge"

mdseries = TexDecl "mdseries"
ssfamily = TexDecl "ssfamily"

-- textXYZ commands should work in math too (use a typeclass)
emph = latexCmdArg "emph"
textrm = latexCmdArg "textrm"
textsf = latexCmdArg "textsf"
texttt = latexCmdArg "texttt"
textmd = latexCmdArg "textmd"
textbf = latexCmdArg "textbf"
textup = latexCmdArg "textup"
textit = latexCmdArg "textit"
textsl = latexCmdArg "textsl"
textsc = latexCmdArg "textsc"
textnormal = latexCmdArg "textnormal"

-- Line and page breaking

-- fragile
linebreak, nolinebreak :: Int -> Latex
linebreak = TexDeclOpt "linebreak" . hint
nolinebreak = TexDeclOpt "nolinebreak" . hint

-- fragile
newline = TexCmdNoArg "newline"
newline' = TexDeclOpt "newline" . LatexSize

-- robust
hyphen = RawTex "{\\-}" -- check if {...} does not cause trouble here

-- robust
hyphenation :: [String] -> ParMode
hyphenation = parCmdArg "hyphenation" . RawTex . unwords -- RawTex is a bit rough here

sloppy = TexDecl "sloppy"
fussy = TexDecl "fussy"

sloppypar = ParEnvironmentPar "sloppypar" []

-- fragile
pagebreak, nopagebreak :: Int -> Latex
(pagebreak, nopagebreak) =
  (TexDeclOpt "pagebreak" . hint . check0to4 "pagebreak"
  ,TexDeclOpt "nopagebreak" . hint . check0to4 "nopagebreak")
  where check0to4 s i | i >= 0 && i <= 4 = i
                      | otherwise        = error $ s ++ ": option must be between 0 and 4 not " ++ show i

-- fragile
samepage = TexDecl "samepage"

-- robust
newpage = ParDecl "newpage"
-- robust
clearpage = ParDecl "clearpage"
-- fragile
cleardoublepage = ParDecl "cleardoublepage"

--- Boxes

{-
class Mbox a where
  -- robust
  mbox :: Latex -> a

  -- fragile
  makebox :: LatexSize -> LatexSize -> Latex -> a

instance Mbox MathItem where
  mbox = MathToLR Nothing Nothing
  makebox = mmakebox

instance Mbox Latex where
  mbox = id

instance Mbox ParMode where
-}

text = MathNeedPackage "amsmath" . MathToLR "text"

-- robust
mbox = latexCmdArg "mbox"

-- fragile
makebox width txt = LatexCmdArgs "makebox" [optional $ LatexSize width,mandatory txt]

-- fragile
makeboxLeft width txt =
  LatexCmdArgs "makebox" [optional $ LatexSize width,optional $ RawTex "l",mandatory txt]

-- fragile
makeboxRight width txt =
  LatexCmdArgs "makebox" [optional $ LatexSize width,optional $ RawTex "r",mandatory txt]

-- robust
fbox = latexCmdArg "fbox"

-- fragile
framebox width txt = LatexCmdArgs "framebox" [optional $ LatexSize width,mandatory txt]

-- fragile
frameboxLeft width txt =
  LatexCmdArgs "framebox" [optional $ LatexSize width,optional $ RawTex "l",mandatory txt]

-- fragile
frameboxRight width txt =
  LatexCmdArgs "framebox" [optional $ LatexSize width,optional $ RawTex "r",mandatory txt]

-- TODO: make a safe version using a monad
-- fragile
unsafeNewsavebox i =
  let bin = UnsafeMakeSaveBin i
  in latexCmdArg "newsavebox" $ LatexSaveBin bin

-- robust
sbox bin txt = LatexCmdArgs "sbox" [mandatory $ LatexSaveBin bin, mandatory txt]

-- fragile
savebox bin width txt =
  LatexCmdArgs "savebox" [mandatory $  LatexSaveBin bin, optional $  LatexSize width,
                          mandatory  txt]

-- fragile
saveboxLeft bin width txt =
  LatexCmdArgs "savebox" [mandatory $  LatexSaveBin bin, optional $  LatexSize width,
                          optional $  RawTex "l", mandatory  txt]

-- fragile
saveboxRight bin width txt =
  LatexCmdArgs "savebox" [mandatory $  LatexSaveBin bin, optional $  LatexSize width,
                          optional $  RawTex "r", mandatory  txt]

-- robust
usebox bin = LatexCmdArgs "usebox" [mandatory $  LatexSaveBin bin]

-- fragile
parbox width txt =
  LatexCmdArgs "parbox" [mandatory $  LatexSize width, mandatory  txt]

-- fragile
parboxTop width txt =
  LatexCmdArgs "parbox" [optional $  RawTex "t", mandatory $  LatexSize width, mandatory  txt]

-- fragile
parboxBot width txt =
  LatexCmdArgs "parbox" [optional $  RawTex "b", mandatory $  LatexSize width, mandatory  txt]

minipage width txt =
  LatexCmdArgs "minipage" [mandatory $  LatexSize width, mandatory $  LatexParMode txt]

minipageTop width txt =
  LatexCmdArgs "minipage" [optional $  RawTex "t", mandatory $  LatexSize width, mandatory  txt]

minipageBot width txt =
  LatexCmdArgs "minipage" [optional $  RawTex "b", mandatory $  LatexSize width, mandatory  txt]

-- fragile
rule width height = LatexCmdArgs "rule" [mandatory $ LatexSize width,mandatory $ LatexSize height]

-- fragile
rule' raise_len width height = LatexCmdArgs "rule" [optional $  LatexSize raise_len
                                                   ,mandatory $ LatexSize width,mandatory $ LatexSize height]

-- fragile
raisebox raise_len txt =
  LatexCmdArgs "raisebox" [mandatory $ LatexSize raise_len,mandatory txt]

-- fragile
raisebox' raise_len height depth txt =
  LatexCmdArgs "raisebox" [mandatory $ LatexSize raise_len
                          ,optional $ LatexSize height,optional $ LatexSize depth,mandatory  txt]

footnote = latexCmdArg "footnote"

caption :: Latex -> Latex
caption txt = LatexCmdArgs "caption" [mandatory txt]
caption' :: String -> Latex -> Latex
caption' lstentry txt = LatexCmdArgs "caption" [optional $ checkentry lstentry, mandatory txt]
  where checkentry x
          | all isAlphaNum x = RawTex x
          | otherwise        = error "caption': restriction to alphanumeric characters for the lstentry"

label = latexCmdArg "label" . LatexKeys . (:[])
ref = latexCmdArg "ref" . LatexKeys . (:[])
pageref = latexCmdArg "pageref" . LatexKeys . (:[])

-- fragile
cite = latexCmdArg "cite" . LatexKeys
cite' txt keys = LatexCmdArgs "cite" [optional txt, mandatory $ LatexKeys keys]

-- fragile
nocite = latexCmdArg "nocite" . LatexKeys

-- sectioning

-- Sectioning commands arguments are 'moving'.
sectioning :: String -> ((Latex -> ParMode), (Star -> Maybe Latex -> Latex -> ParMode))
sectioning name = (sect, sect')
  where sect = sect' NoStar Nothing
        sect' star opt arg = ParCmdArgs (name ++ addstar star)
                                        (maybeToList (fmap optional opt) ++ [mandatory arg])
        addstar Star   = "*"
        addstar NoStar = ""

part, chapter, section, subsection,  subsubsection :: Latex -> ParMode
part', chapter', section', subsection', subsubsection' :: Star -> Maybe Latex -> Latex -> ParMode

(part, part')       = sectioning "part"
(chapter, chapter') = sectioning "chapter"
(section, section') = sectioning "section"
(subsection, subsection')       = sectioning "subsection"
(subsubsection, subsubsection') = sectioning "subsubsection"
(paragraph, paragraph')         = sectioning "paragraph"
(subparagraph, subparagraph')   = sectioning "subparagraph"

-- | Don't confuse 'paragraph' with 'para', 'para' is to make a paragraph,
-- 'paragraph' is to group a set of paragraphs.
para = Para

bibliography = latexCmdArg "bibliography"
bibliographystyle = latexCmdArg "bibliographystyle"
thispagestyle = latexCmdArg "thispagestyle"

-- should be \setlength{a}{b}{c} ...
setlength = latexCmdArg "setlength"

listLikeEnv name items =
  ParEnvironmentPar name [] $ mconcat $ map mkItem items
  where mkItem (Item Nothing contents)    = ParDecl "item" <> contents
        mkItem (Item (Just lab) contents) = ParDeclOpt "item" lab <> contents

item :: ParMode -> Item
item = Item Nothing

item' :: Latex -> ParMode -> Item
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
quote :: Latex -> ParMode
quote = ParEnvironmentLR "quote"
quotation = ParEnvironmentPar "quotation"
verse = ParEnvironmentPar "verse"

-- The array and tablular Environments

-- use a monad to report errors
tabular specs rows = Tabular specs $ checkRows specs rows

array specs rows = MathArray specs $ checkRows specs rows

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
  mathCmdsArg, texDecls, mathDecls :: [String]
  mathCmds :: [(String, String)]

  mathCmds =
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

  mathCmdsArg =
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

  mathDecls = ["displaystyle", "textstyle", "scriptstyle", "scriptscriptstyle"
              ,"mit","cal"
              ]

  lowerName :: String -> Name
  lowerName name | isLower (head name) = mkName name
                | otherwise           = mkName $ '_':name

  mkMathCmd (name, _) =
    let lname = lowerName name
    in
    [sigD lname [t| MathItem |]
    , valD (varP lname) (normalB [| mathCmd $(stringE name) |]) []
    ]

  mkMathCmdArg name =
    let lname = lowerName name in
    [sigD lname [t| MathItem -> MathItem |]
    , valD (varP lname) (normalB [| mathCmdArg $(stringE name) |]) []
    ]

  mkTexDecl name =
    let lname = lowerName name in
    [sigD lname [t| Latex |]
    , valD (varP lname) (normalB [| TexDecl $(stringE name) [] |]) []
    ]

  mkMathDecl name =
    let lname = lowerName name in
    [sigD lname [t| MathItem |]
     , valD (varP lname) (normalB [| MathDecl $(stringE name) [] |]) []
    ]

  mkList name ty names =
    [sigD name ty
    ,valD (varP name) (normalB (listE $ map (varE . lowerName) names)) []]

  d = sequence $ concat $ concat
      [ map mkMathCmd mathCmds
      , map mkMathCmdArg mathCmdsArg
      , map mkTexDecl texDecls
      , map mkMathDecl mathDecls
      , [mkList (mkName "mathItems") [t| [MathItem] |] $ (map fst mathCmds ++ mathDecls)]
      , [mkList (mkName "mathCmdsArg") [t| [MathItem -> MathItem] |] mathCmdsArg]
      , [mkList (mkName "texDecls") [t| [Latex] |] texDecls]
      ]
  in do dd <- d
        runIO $ writeFile "/tmp/a.hs" $ pprint dd
        return []
 )
 -}

{- This chunk was generated by the previous TH splice.
   The lists of all commands are manually maintained though. -}
usepackage = PreambleCmdArg "usepackage"
title = PreambleCmdArg "title"
subtitle = PreambleCmdArg "subtitle"
date = PreambleCmdArg "date"
author = PreambleCmdArg "author"
and = TexCmdNoArg "and"
authors = author . mconcat . intersperse and
institute = PreambleCmdArg "institute"


lbrace :: MathItem
lbrace = mathCmd "lbrace"
rbrace :: MathItem
rbrace = mathCmd "rbrace"
space :: MathItem
space = mathCmd "space"
at :: MathItem
at = mathCmd "at"
in_ :: MathItem
in_ = mathCmd "in_"
forall_ :: MathItem
forall_ = mathCmd "forall_"
mthinspace :: MathItem
mthinspace = mathCmd "mthinspace"
mnegthinspace :: MathItem
mnegthinspace = mathCmd "mnegthinspace"
mmediumspace :: MathItem
mmediumspace = mathCmd "mmediumspace"
mthickspace :: MathItem
mthickspace = mathCmd "mthickspace"
msup :: MathItem
msup = mathCmd "msup"
alpha :: MathItem
alpha = mathCmd "alpha"
beta :: MathItem
beta = mathCmd "beta"
chi :: MathItem
chi = mathCmd "chi"
delta :: MathItem
delta = mathCmd "delta"
_Delta :: MathItem
_Delta = mathCmd "Delta"
epsilon :: MathItem
epsilon = mathCmd "epsilon"
varepsilon :: MathItem
varepsilon = mathCmd "varepsilon"
eta :: MathItem
eta = mathCmd "eta"
gamma :: MathItem
gamma = mathCmd "gamma"
_Gamma :: MathItem
_Gamma = mathCmd "Gamma"
iota :: MathItem
iota = mathCmd "iota"
kappa :: MathItem
kappa = mathCmd "kappa"
lambda :: MathItem
lambda = mathCmd "lambda"
_Lambda :: MathItem
_Lambda = mathCmd "Lambda"
mu :: MathItem
mu = mathCmd "mu"
nu :: MathItem
nu = mathCmd "nu"
omega :: MathItem
omega = mathCmd "omega"
_Omega :: MathItem
_Omega = mathCmd "Omega"
phi :: MathItem
phi = mathCmd "phi"
varphi :: MathItem
varphi = mathCmd "varphi"
_Phi :: MathItem
_Phi = mathCmd "Phi"
pi :: MathItem
pi = mathCmd "pi"
_Pi :: MathItem
_Pi = mathCmd "Pi"
psi :: MathItem
psi = mathCmd "psi"
rho :: MathItem
rho = mathCmd "rho"
sigma :: MathItem
sigma = mathCmd "sigma"
_Sigma :: MathItem
_Sigma = mathCmd "Sigma"
tau :: MathItem
tau = mathCmd "tau"
theta :: MathItem
theta = mathCmd "theta"
vartheta :: MathItem
vartheta = mathCmd "vartheta"
_Theta :: MathItem
_Theta = mathCmd "Theta"
upsilon :: MathItem
upsilon = mathCmd "upsilon"
xi :: MathItem
xi = mathCmd "xi"
_Xi :: MathItem
_Xi = mathCmd "Xi"
zeta :: MathItem
zeta = mathCmd "zeta"
backslash :: MathItem
backslash = mathCmd "backslash"
times :: MathItem
times = mathCmd "times"
divide :: MathItem
divide = mathCmd "divide"
circ :: MathItem
circ = mathCmd "circ"
oplus :: MathItem
oplus = mathCmd "oplus"
otimes :: MathItem
otimes = mathCmd "otimes"
sum :: MathItem
sum = mathCmd "sum"
prod :: MathItem
prod = mathCmd "prod"
wedge :: MathItem
wedge = mathCmd "wedge"
bigwedge :: MathItem
bigwedge = mathCmd "bigwedge"
vee :: MathItem
vee = mathCmd "vee"
bigvee :: MathItem
bigvee = mathCmd "bigvee"
cup :: MathItem
cup = mathCmd "cup"
bigcup :: MathItem
bigcup = mathCmd "bigcup"
cap :: MathItem
cap = mathCmd "cap"
bigcap :: MathItem
bigcap = mathCmd "bigcap"
ne :: MathItem
ne = mathCmd "ne"
le :: MathItem
le = mathCmd "le"
leq :: MathItem
leq = mathCmd "leq"
ge :: MathItem
ge = mathCmd "ge"
geq :: MathItem
geq = mathCmd "geq"
prec :: MathItem
prec = mathCmd "prec"
succ :: MathItem
succ = mathCmd "succ"
notin :: MathItem
notin = mathCmd "notin"
subset :: MathItem
subset = mathCmd "subset"
supset :: MathItem
supset = mathCmd "supset"
subseteq :: MathItem
subseteq = mathCmd "subseteq"
supseteq :: MathItem
supseteq = mathCmd "supseteq"
equiv :: MathItem
equiv = mathCmd "equiv"
cong :: MathItem
cong = mathCmd "cong"
approx :: MathItem
approx = mathCmd "approx"
propto :: MathItem
propto = mathCmd "propto"
neg :: MathItem
neg = mathCmd "neg"
implies :: MathItem
implies = mathCmd "implies"
iff :: MathItem
iff = mathCmd "iff"
exists :: MathItem
exists = mathCmd "exists"
bot :: MathItem
bot = mathCmd "bot"
top :: MathItem
top = mathCmd "top"
vdash :: MathItem
vdash = mathCmd "vdash"
models :: MathItem
models = mathCmd "models"
langle :: MathItem
langle = mathCmd "langle"
rangle :: MathItem
rangle = mathCmd "rangle"
int :: MathItem
int = mathCmd "int"
oint :: MathItem
oint = mathCmd "oint"
partial :: MathItem
partial = mathCmd "partial"
nabla :: MathItem
nabla = mathCmd "nabla"
pm :: MathItem
pm = mathCmd "pm"
emptyset :: MathItem
emptyset = mathCmd "emptyset"
infty :: MathItem
infty = mathCmd "infty"
aleph :: MathItem
aleph = mathCmd "aleph"
ldots :: MathItem
ldots = mathCmd "ldots"
cdots :: MathItem
cdots = mathCmd "cdots"
vdots :: MathItem
vdots = mathCmd "vdots"
ddots :: MathItem
ddots = mathCmd "ddots"
quad :: MathItem
quad = mathCmd "quad"
diamond :: MathItem
diamond = mathCmd "diamond"
square :: MathItem
square = mathCmd "square"
lfloor :: MathItem
lfloor = mathCmd "lfloor"
rfloor :: MathItem
rfloor = mathCmd "rfloor"
lceiling :: MathItem
lceiling = mathCmd "lceiling"
rceiling :: MathItem
rceiling = mathCmd "rceiling"
sin :: MathItem
sin = mathCmd "sin"
cos :: MathItem
cos = mathCmd "cos"
tan :: MathItem
tan = mathCmd "tan"
csc :: MathItem
csc = mathCmd "csc"
sec :: MathItem
sec = mathCmd "sec"
cot :: MathItem
cot = mathCmd "cot"
sinh :: MathItem
sinh = mathCmd "sinh"
cosh :: MathItem
cosh = mathCmd "cosh"
tanh :: MathItem
tanh = mathCmd "tanh"
log :: MathItem
log = mathCmd "log"
ln :: MathItem
ln = mathCmd "ln"
det :: MathItem
det = mathCmd "det"
dim :: MathItem
dim = mathCmd "dim"
lim :: MathItem
lim = mathCmd "lim"
mod :: MathItem
mod = mathCmd "mod"
gcd :: MathItem
gcd = mathCmd "gcd"
lcm :: MathItem
lcm = mathCmd "lcm"
liminf :: MathItem
liminf = mathCmd "liminf"
inf :: MathItem
inf = mathCmd "inf"
limsup :: MathItem
limsup = mathCmd "limsup"
max :: MathItem
max = mathCmd "max"
min :: MathItem
min = mathCmd "min"
_Pr :: MathItem
_Pr = mathCmd "Pr"
uparrow :: MathItem
uparrow = mathCmd "uparrow"
downarrow :: MathItem
downarrow = mathCmd "downarrow"
rightarrow :: MathItem
rightarrow = mathCmd "rightarrow"
to :: MathItem
to = mathCmd "to"
leftarrow :: MathItem
leftarrow = mathCmd "leftarrow"
leftrightarrow :: MathItem
leftrightarrow = mathCmd "leftrightarrow"
_Rightarrow :: MathItem
_Rightarrow = mathCmd "Rightarrow"
_Leftarrow :: MathItem
_Leftarrow = mathCmd "Leftarrow"
_Leftrightarrow :: MathItem
_Leftrightarrow = mathCmd "Leftrightarrow"
mathbf :: MathItem -> MathItem
mathbf = mathCmdArg "mathbf"
mathbb :: MathItem -> MathItem
mathbb = mathCmdArg "mathbb"
mathcal :: MathItem -> MathItem
mathcal = mathCmdArg "mathcal"
mathtt :: MathItem -> MathItem
mathtt = mathCmdArg "mathtt"
mathfrak :: MathItem -> MathItem
mathfrak = mathCmdArg "mathfrak"
pmod :: MathItem -> MathItem
pmod = mathCmdArg "pmod"
tilde :: MathItem -> MathItem
tilde = mathCmdArg "tilde"
hat :: MathItem -> MathItem
hat = mathCmdArg "hat"
check :: MathItem -> MathItem
check = mathCmdArg "check"
breve :: MathItem -> MathItem
breve = mathCmdArg "breve"
acute :: MathItem -> MathItem
acute = mathCmdArg "acute"
grave :: MathItem -> MathItem
grave = mathCmdArg "grave"
bar :: MathItem -> MathItem
bar = mathCmdArg "bar"
vec :: MathItem -> MathItem
vec = mathCmdArg "vec"
dot :: MathItem -> MathItem
dot = mathCmdArg "dot"
ddot :: MathItem -> MathItem
ddot = mathCmdArg "ddot"
overbrace :: MathItem -> MathItem
overbrace = mathCmdArg "overbrace"
underbrace :: MathItem -> MathItem
underbrace = mathCmdArg "underbrace"
overline :: MathItem -> MathItem
overline = mathCmdArg "overline"
underline :: MathItem -> MathItem
underline = mathCmdArg "underline"
widehat :: MathItem -> MathItem
widehat = mathCmdArg "widehat"
widetilde :: MathItem -> MathItem
widetilde = mathCmdArg "widetilde"
imath :: MathItem -> MathItem
imath = mathCmdArg "imath"
jmath :: MathItem -> MathItem
jmath = mathCmdArg "jmath"
{-# DEPRECATED em "Use emph instead" #-}
em :: Latex
em = TexDecl "em"
{-# DEPRECATED bf "Use textbf instead" #-}
bf :: Latex
bf = TexDecl "bf"
{-# DEPRECATED sf "Use textsf instead" #-}
sf :: Latex
sf = TexDecl "sf"
{-# DEPRECATED sl "Use textsl instead" #-}
sl :: Latex
sl = TexDecl "sl"
{-# DEPRECATED sc "Use textsc instead" #-}
sc :: Latex
sc = TexDecl "sc"
{-# DEPRECATED it "Use textit instead" #-}
it :: Latex
it = TexDecl "it"
{-# DEPRECATED tt "Use texttt instead" #-}
tt :: Latex
tt = TexDecl "tt"
displaystyle :: MathItem
displaystyle = MathDecl "displaystyle"
textstyle :: MathItem
textstyle = MathDecl "textstyle"
scriptstyle :: MathItem
scriptstyle = MathDecl "scriptstyle"
scriptscriptstyle :: MathItem
scriptscriptstyle = MathDecl "scriptscriptstyle"
mit :: MathItem
mit = MathDecl "mit"
cal :: MathItem
cal = MathDecl "cal"
eq :: MathItem
eq = RawMath "{=}"

bmod :: MathItem -> MathItem -> MathItem
bmod = MathBinOp "bmod"

mathItems :: [MathItem]
mathItems =
  [lbrace, rbrace, space, at, in_, forall_, mthinspace, mnegthinspace, mmediumspace,
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
   rightarrow, to, leftarrow, leftrightarrow, _Rightarrow, _Leftarrow, _Leftrightarrow,
   displaystyle, textstyle, scriptstyle, scriptscriptstyle, mit, cal
  -- maually added
  ,eq
  ]

mathCmdsArg :: [MathItem -> MathItem]
mathCmdsArg = [mathbf, mathbb, mathcal, mathtt, mathfrak, pmod, tilde, hat, check,
                breve, acute, grave, bar, vec, dot, ddot, overbrace, underbrace, overline,
                underline, widehat, widetilde, imath, jmath
                -- maually added
               ,negate, sqrt
               ]

mathBinOp :: [MathItem -> MathItem -> MathItem]
mathBinOp = [(+),(-),(*),bmod]

texDecls :: [Latex]
texDecls = [em, bf, sf, sl, sc, it, tt]

-- beamer
-- alert
-- AtBeginSubsection, AtBeginSection
only = latexCmdArg "only"
usetheme = PreambleCmdArg "usetheme"
usefontthem = PreambleCmdArg "usefontthem"
