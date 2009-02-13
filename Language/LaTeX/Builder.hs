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
instance Group MathsItem where group = MathsGroup

mandatory, optional :: a -> Arg a
mandatory x = Arg Mandatory x
optional x = Arg Optional x

mathsCmdArg :: String -> MathsItem -> MathsItem
mathsCmdArg x y = MathsCmdArgs x [mandatory y]

parCmdArg :: String -> Latex -> ParMode
parCmdArg x y = ParCmdArgs x [mandatory y]

latexCmdArg :: String -> Latex -> Latex
latexCmdArg x y = LatexCmdArgs x [mandatory y]

mathsCmd :: String -> MathsItem
mathsCmd x = MathsCmdArgs x []

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
frac x y = MathsCmdArgs "frac" [mandatory x,mandatory y]
stackrel x y = MathsCmdArgs "stackrel" [mandatory x,mandatory y]

sqrt :: MathsItem -> MathsItem
sqrt x = MathsCmdArgs "sqrt" [mandatory x]

sqrt' :: MathsItem -> MathsItem -> MathsItem
sqrt' n x = MathsCmdArgs "sqrt" [optional n, mandatory x]

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

mchar = RawMaths . mchar'

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

-- textXYZ commands should work in maths too (use a typeclass)
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

instance Mbox MathsItem where
  mbox = MathsToLR Nothing Nothing
  makebox = mmakebox

instance Mbox Latex where
  mbox = id

instance Mbox ParMode where
-}

text = MathsNeedsPackage "amsmath" . MathsToLR "text"

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
    , valD (varP lname) (normalB [| mathsCmd $(stringE name) |]) []
    ]

  mkMathsCmdArg name =
    let lname = lowerName name in
    [sigD lname [t| MathsItem -> MathsItem |]
    , valD (varP lname) (normalB [| mathsCmdArg $(stringE name) |]) []
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
      , [mkList (mkName "mathsItems") [t| [MathsItem] |] $ (map fst mathsCmds ++ mathsDecls)]
      , [mkList (mkName "mathsCmdsArg") [t| [MathsItem -> MathsItem] |] mathsCmdsArg]
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


lbrace :: MathsItem
lbrace = mathsCmd "lbrace"
rbrace :: MathsItem
rbrace = mathsCmd "rbrace"
space :: MathsItem
space = mathsCmd "space"
at :: MathsItem
at = mathsCmd "at"
in_ :: MathsItem
in_ = mathsCmd "in_"
forall_ :: MathsItem
forall_ = mathsCmd "forall_"
mthinspace :: MathsItem
mthinspace = mathsCmd "mthinspace"
mnegthinspace :: MathsItem
mnegthinspace = mathsCmd "mnegthinspace"
mmediumspace :: MathsItem
mmediumspace = mathsCmd "mmediumspace"
mthickspace :: MathsItem
mthickspace = mathsCmd "mthickspace"
msup :: MathsItem
msup = mathsCmd "msup"
alpha :: MathsItem
alpha = mathsCmd "alpha"
beta :: MathsItem
beta = mathsCmd "beta"
chi :: MathsItem
chi = mathsCmd "chi"
delta :: MathsItem
delta = mathsCmd "delta"
_Delta :: MathsItem
_Delta = mathsCmd "Delta"
epsilon :: MathsItem
epsilon = mathsCmd "epsilon"
varepsilon :: MathsItem
varepsilon = mathsCmd "varepsilon"
eta :: MathsItem
eta = mathsCmd "eta"
gamma :: MathsItem
gamma = mathsCmd "gamma"
_Gamma :: MathsItem
_Gamma = mathsCmd "Gamma"
iota :: MathsItem
iota = mathsCmd "iota"
kappa :: MathsItem
kappa = mathsCmd "kappa"
lambda :: MathsItem
lambda = mathsCmd "lambda"
_Lambda :: MathsItem
_Lambda = mathsCmd "Lambda"
mu :: MathsItem
mu = mathsCmd "mu"
nu :: MathsItem
nu = mathsCmd "nu"
omega :: MathsItem
omega = mathsCmd "omega"
_Omega :: MathsItem
_Omega = mathsCmd "Omega"
phi :: MathsItem
phi = mathsCmd "phi"
varphi :: MathsItem
varphi = mathsCmd "varphi"
_Phi :: MathsItem
_Phi = mathsCmd "Phi"
pi :: MathsItem
pi = mathsCmd "pi"
_Pi :: MathsItem
_Pi = mathsCmd "Pi"
psi :: MathsItem
psi = mathsCmd "psi"
rho :: MathsItem
rho = mathsCmd "rho"
sigma :: MathsItem
sigma = mathsCmd "sigma"
_Sigma :: MathsItem
_Sigma = mathsCmd "Sigma"
tau :: MathsItem
tau = mathsCmd "tau"
theta :: MathsItem
theta = mathsCmd "theta"
vartheta :: MathsItem
vartheta = mathsCmd "vartheta"
_Theta :: MathsItem
_Theta = mathsCmd "Theta"
upsilon :: MathsItem
upsilon = mathsCmd "upsilon"
xi :: MathsItem
xi = mathsCmd "xi"
_Xi :: MathsItem
_Xi = mathsCmd "Xi"
zeta :: MathsItem
zeta = mathsCmd "zeta"
backslash :: MathsItem
backslash = mathsCmd "backslash"
times :: MathsItem
times = mathsCmd "times"
divide :: MathsItem
divide = mathsCmd "divide"
circ :: MathsItem
circ = mathsCmd "circ"
oplus :: MathsItem
oplus = mathsCmd "oplus"
otimes :: MathsItem
otimes = mathsCmd "otimes"
sum :: MathsItem
sum = mathsCmd "sum"
prod :: MathsItem
prod = mathsCmd "prod"
wedge :: MathsItem
wedge = mathsCmd "wedge"
bigwedge :: MathsItem
bigwedge = mathsCmd "bigwedge"
vee :: MathsItem
vee = mathsCmd "vee"
bigvee :: MathsItem
bigvee = mathsCmd "bigvee"
cup :: MathsItem
cup = mathsCmd "cup"
bigcup :: MathsItem
bigcup = mathsCmd "bigcup"
cap :: MathsItem
cap = mathsCmd "cap"
bigcap :: MathsItem
bigcap = mathsCmd "bigcap"
ne :: MathsItem
ne = mathsCmd "ne"
le :: MathsItem
le = mathsCmd "le"
leq :: MathsItem
leq = mathsCmd "leq"
ge :: MathsItem
ge = mathsCmd "ge"
geq :: MathsItem
geq = mathsCmd "geq"
prec :: MathsItem
prec = mathsCmd "prec"
succ :: MathsItem
succ = mathsCmd "succ"
notin :: MathsItem
notin = mathsCmd "notin"
subset :: MathsItem
subset = mathsCmd "subset"
supset :: MathsItem
supset = mathsCmd "supset"
subseteq :: MathsItem
subseteq = mathsCmd "subseteq"
supseteq :: MathsItem
supseteq = mathsCmd "supseteq"
equiv :: MathsItem
equiv = mathsCmd "equiv"
cong :: MathsItem
cong = mathsCmd "cong"
approx :: MathsItem
approx = mathsCmd "approx"
propto :: MathsItem
propto = mathsCmd "propto"
neg :: MathsItem
neg = mathsCmd "neg"
implies :: MathsItem
implies = mathsCmd "implies"
iff :: MathsItem
iff = mathsCmd "iff"
exists :: MathsItem
exists = mathsCmd "exists"
bot :: MathsItem
bot = mathsCmd "bot"
top :: MathsItem
top = mathsCmd "top"
vdash :: MathsItem
vdash = mathsCmd "vdash"
models :: MathsItem
models = mathsCmd "models"
langle :: MathsItem
langle = mathsCmd "langle"
rangle :: MathsItem
rangle = mathsCmd "rangle"
int :: MathsItem
int = mathsCmd "int"
oint :: MathsItem
oint = mathsCmd "oint"
partial :: MathsItem
partial = mathsCmd "partial"
nabla :: MathsItem
nabla = mathsCmd "nabla"
pm :: MathsItem
pm = mathsCmd "pm"
emptyset :: MathsItem
emptyset = mathsCmd "emptyset"
infty :: MathsItem
infty = mathsCmd "infty"
aleph :: MathsItem
aleph = mathsCmd "aleph"
ldots :: MathsItem
ldots = mathsCmd "ldots"
cdots :: MathsItem
cdots = mathsCmd "cdots"
vdots :: MathsItem
vdots = mathsCmd "vdots"
ddots :: MathsItem
ddots = mathsCmd "ddots"
quad :: MathsItem
quad = mathsCmd "quad"
diamond :: MathsItem
diamond = mathsCmd "diamond"
square :: MathsItem
square = mathsCmd "square"
lfloor :: MathsItem
lfloor = mathsCmd "lfloor"
rfloor :: MathsItem
rfloor = mathsCmd "rfloor"
lceiling :: MathsItem
lceiling = mathsCmd "lceiling"
rceiling :: MathsItem
rceiling = mathsCmd "rceiling"
sin :: MathsItem
sin = mathsCmd "sin"
cos :: MathsItem
cos = mathsCmd "cos"
tan :: MathsItem
tan = mathsCmd "tan"
csc :: MathsItem
csc = mathsCmd "csc"
sec :: MathsItem
sec = mathsCmd "sec"
cot :: MathsItem
cot = mathsCmd "cot"
sinh :: MathsItem
sinh = mathsCmd "sinh"
cosh :: MathsItem
cosh = mathsCmd "cosh"
tanh :: MathsItem
tanh = mathsCmd "tanh"
log :: MathsItem
log = mathsCmd "log"
ln :: MathsItem
ln = mathsCmd "ln"
det :: MathsItem
det = mathsCmd "det"
dim :: MathsItem
dim = mathsCmd "dim"
lim :: MathsItem
lim = mathsCmd "lim"
mod :: MathsItem
mod = mathsCmd "mod"
gcd :: MathsItem
gcd = mathsCmd "gcd"
lcm :: MathsItem
lcm = mathsCmd "lcm"
liminf :: MathsItem
liminf = mathsCmd "liminf"
inf :: MathsItem
inf = mathsCmd "inf"
limsup :: MathsItem
limsup = mathsCmd "limsup"
max :: MathsItem
max = mathsCmd "max"
min :: MathsItem
min = mathsCmd "min"
_Pr :: MathsItem
_Pr = mathsCmd "Pr"
uparrow :: MathsItem
uparrow = mathsCmd "uparrow"
downarrow :: MathsItem
downarrow = mathsCmd "downarrow"
rightarrow :: MathsItem
rightarrow = mathsCmd "rightarrow"
to :: MathsItem
to = mathsCmd "to"
leftarrow :: MathsItem
leftarrow = mathsCmd "leftarrow"
leftrightarrow :: MathsItem
leftrightarrow = mathsCmd "leftrightarrow"
_Rightarrow :: MathsItem
_Rightarrow = mathsCmd "Rightarrow"
_Leftarrow :: MathsItem
_Leftarrow = mathsCmd "Leftarrow"
_Leftrightarrow :: MathsItem
_Leftrightarrow = mathsCmd "Leftrightarrow"
mathbf :: MathsItem -> MathsItem
mathbf = mathsCmdArg "mathbf"
mathbb :: MathsItem -> MathsItem
mathbb = mathsCmdArg "mathbb"
mathcal :: MathsItem -> MathsItem
mathcal = mathsCmdArg "mathcal"
mathtt :: MathsItem -> MathsItem
mathtt = mathsCmdArg "mathtt"
mathfrak :: MathsItem -> MathsItem
mathfrak = mathsCmdArg "mathfrak"
pmod :: MathsItem -> MathsItem
pmod = mathsCmdArg "pmod"
tilde :: MathsItem -> MathsItem
tilde = mathsCmdArg "tilde"
hat :: MathsItem -> MathsItem
hat = mathsCmdArg "hat"
check :: MathsItem -> MathsItem
check = mathsCmdArg "check"
breve :: MathsItem -> MathsItem
breve = mathsCmdArg "breve"
acute :: MathsItem -> MathsItem
acute = mathsCmdArg "acute"
grave :: MathsItem -> MathsItem
grave = mathsCmdArg "grave"
bar :: MathsItem -> MathsItem
bar = mathsCmdArg "bar"
vec :: MathsItem -> MathsItem
vec = mathsCmdArg "vec"
dot :: MathsItem -> MathsItem
dot = mathsCmdArg "dot"
ddot :: MathsItem -> MathsItem
ddot = mathsCmdArg "ddot"
overbrace :: MathsItem -> MathsItem
overbrace = mathsCmdArg "overbrace"
underbrace :: MathsItem -> MathsItem
underbrace = mathsCmdArg "underbrace"
overline :: MathsItem -> MathsItem
overline = mathsCmdArg "overline"
underline :: MathsItem -> MathsItem
underline = mathsCmdArg "underline"
widehat :: MathsItem -> MathsItem
widehat = mathsCmdArg "widehat"
widetilde :: MathsItem -> MathsItem
widetilde = mathsCmdArg "widetilde"
imath :: MathsItem -> MathsItem
imath = mathsCmdArg "imath"
jmath :: MathsItem -> MathsItem
jmath = mathsCmdArg "jmath"
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
displaystyle :: MathsItem
displaystyle = MathsDecl "displaystyle"
textstyle :: MathsItem
textstyle = MathsDecl "textstyle"
scriptstyle :: MathsItem
scriptstyle = MathsDecl "scriptstyle"
scriptscriptstyle :: MathsItem
scriptscriptstyle = MathsDecl "scriptscriptstyle"
mit :: MathsItem
mit = MathsDecl "mit"
cal :: MathsItem
cal = MathsDecl "cal"
eq :: MathsItem
eq = RawMaths "{=}"

bmod :: MathsItem -> MathsItem -> MathsItem
bmod = MathsBinOp "bmod"

mathsItems :: [MathsItem]
mathsItems =
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

mathsCmdsArg :: [MathsItem -> MathsItem]
mathsCmdsArg = [mathbf, mathbb, mathcal, mathtt, mathfrak, pmod, tilde, hat, check,
                breve, acute, grave, bar, vec, dot, ddot, overbrace, underbrace, overline,
                underline, widehat, widetilde, imath, jmath
                -- maually added
               ,negate, sqrt
               ]

mathsBinOp :: [MathsItem -> MathsItem -> MathsItem]
mathsBinOp = [(+),(-),(*),bmod]

texDecls :: [Latex]
texDecls = [em, bf, sf, sl, sc, it, tt]

-- beamer
-- alert
-- AtBeginSubsection, AtBeginSection
only = latexCmdArg "only"
usetheme = PreambleCmdArg "usetheme"
usefontthem = PreambleCmdArg "usefontthem"
