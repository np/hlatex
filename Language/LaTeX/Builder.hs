module Language.LaTeX.Builder where

import Prelude hiding (sqrt, min, max, lcm, gcd, log, mod, tanh, cosh, tan, sinh,
                       sin, cos, succ, sum, pi, mapM)
import Data.List hiding (sum, and, group)
import qualified Data.List as L
import Data.Maybe
import Data.Ratio
import Data.Monoid
import Data.Char
import Data.Traversable (sequenceA, mapM)
import Data.String (IsString(..))
import Control.Applicative hiding (optional)
import Control.Monad hiding (mapM)
import Control.Monad.Error (throwError)
import Control.Monad.Writer (Writer, execWriter, tell)
import Control.Arrow

import Language.LaTeX.Types
import Language.LaTeX.Builder.MonoidUtils


{- TODO:
    - robust/fragile/moving
    - tracking savebin in the monad?
    - generating a doc with examples:
         [...("sum", [| sum<>sub(i<>eq<>0)<>sup infty<>i<>sup 2 |])...]
    - pictures
 -}

{-
import Prelude (writeFile, id, Monad(..), fst)
import Language.Haskell.TH
-}

group :: LatexItem -> LatexItem
group = liftM TexGroup

infixr 0 !$
infixr 0 $?
infixr 0 !$?

(!$) :: Monoid b => (a -> b) -> a -> Writer b ()
(!$) f x = tell $ f x

($?) :: (a -> b) -> Writer a () -> b
($?) f m = f $ execWriter m

-- NOTE: This combinator seems pretty promising...
(!$?) :: Monoid b => (a -> b) -> Writer a () -> Writer b ()
(!$?) f m = tell $ f $ execWriter m

noArg :: Arg a
noArg = NoArg
mandatory, optional :: a -> Arg a
mandatory = Mandatory
optional = Optional
coordinates :: a -> a -> Arg a
coordinates = Coordinates
optionals :: [a] -> Arg a
optionals = Optionals
packageDependency :: PackageName -> Arg a
packageDependency = PackageDependency

math :: MathItem -> LatexItem
math = liftM MathInline

displaymath :: MathItem -> ParItem
displaymath = liftM DisplayMath

rawDecls :: [TexDecl] -> LatexItem
rawDecls = fmap TexDecls . sequenceA

decls :: [TexDecl] -> LatexItem -> LatexItem
decls ds x = group (rawDecls ds <> x)

decl :: TexDecl -> LatexItem -> LatexItem
decl d = decls [d]

texDecl :: String -> TexDecl
texDecl s = pure $ TexDcl s []

texDecl' :: String -> [Arg LatexItem] -> TexDecl
texDecl' s opt = TexDcl s <$> mapM sequenceA opt

texDeclOpt :: String -> LatexItem -> TexDecl
texDeclOpt s opt = TexDcl s <$> ((:[]) . optional <$> opt)

parCmdArgs :: String -> [Arg LatexItem] -> ParItem
parCmdArgs x ys = ParCmdArgs x <$> mapM sequenceA ys

parCmdArg :: String -> LatexItem -> ParItem
parCmdArg x y = parCmdArgs x [mandatory y]

latexCmdArgs :: String -> [Arg LatexItem] -> LatexItem
latexCmdArgs x ys = LatexCmdArgs x <$> mapM sequenceA ys

latexCmdArg :: String -> LatexItem -> LatexItem
latexCmdArg x y = latexCmdArgs x [mandatory y]

preambleCmdArgs :: String -> [Arg LatexItem] -> PreambleItem
preambleCmdArgs x ys = PreambleCmdArgs x <$> mapM sequenceA ys

preambleCmdArg :: String -> LatexItem -> PreambleItem
preambleCmdArg x y = preambleCmdArgs x [mandatory y]

rawPreamble :: String -> PreambleItem
rawPreamble = pure . RawPreamble

size :: LatexSize -> LatexItem
size = pure . LatexSize

pkgName :: String -> PackageName
pkgName = PkgName

bool :: Bool -> LatexItem
bool True  = rawTex "true"
bool False = rawTex "false"

coord :: Coord -> LatexItem
coord = pure . LatexCoord

latexSaveBin :: SaveBin -> LatexItem
latexSaveBin = pure . LatexSaveBin

latexEnvironment :: String -> [Arg LatexItem] -> LatexItem -> LatexItem
latexEnvironment x ys = liftM2 (Environment x) $ mapM sequenceA ys

parEnvironmentPar :: String -> [Arg LatexItem] -> ParItem -> ParItem
parEnvironmentPar x ys = liftM2 (ParEnvironmentPar x) $ mapM sequenceA ys

figureLike :: String -> [LocSpec] -> ParItem -> ParItem
figureLike x y = liftM $ FigureLike x y

listLikeEnv :: String -> [ListItem] -> ParItem
listLikeEnv name items =
  parEnvironmentPar name [] (mconcat <$> mapM (fmap mkItem) items)
  where mkItem (ListItm Nothing contents)    = ParCmdArgs "item" [] <> contents
        mkItem (ListItm (Just lab) contents) = ParCmdArgs "item" [optional lab] <> contents

rawTex :: String -> LatexItem
rawTex = pure . RawTex

texCmdNoArg :: String -> LatexItem
texCmdNoArg = pure . TexCmdNoArg

latexKey :: Key -> LatexItem
latexKey = pure . LatexKeys . (:[])

latexKeys :: [Key] -> LatexItem
latexKeys = pure . LatexKeys

root :: PreambleItem -> LatexM Document -> LatexM Root
root = liftM2 Root

document :: ParItem -> LatexM Document
document = liftM Document

dash1, dash2, dash3, nbsp :: LatexItem
dash1 = rawTex "{-}"
dash2 = rawTex "{--}"
dash3 = rawTex "{---}"

nbsp = rawTex "{~}"
-- Do we want to treat '\160' as an nbsp too?

-- | 'sep' is like '{}' in LaTeX, it allows to force letters to be treaten separately,
-- for instance to separate the two 'f's in shelfful.
sep :: LatexItem
sep = group mempty

space :: LatexItem
space = rawTex "{ }"

{- GHC bug with OverloadedStrings
f "" = 1
f ('a':_) = 2
f _ = 3
-}

protector :: (String -> LatexItem) -> String -> LatexItem
protector _ []        = mempty
protector f ('\n':xs) = newline <> protector f xs
protector f (' ':xs)  = uncurry (<>) $ (hspace_ . (+1) . length *** protector f) $ break (/=' ') xs
  where hspace_ n = mbox . hspace . Em $ 1%2 * fromIntegral n
protector f (x:xs)    = uncurry (<>) $ (f . (x :) *** protector f) $ break (`elem` " \n") xs

protect :: String -> LatexItem
protect = protector hstring

href :: LatexItem -> LatexItem -> LatexItem
href x y = latexCmdArgs "href" [mandatory x,mandatory y]
person :: String -> String -> LatexItem
person name email = href (hstring ("mailto:"++email)) (hstring name)

inch, pt, cm, mm, ex, pc, sp, bp, dd, cc, mu :: Rational -> LatexSize
pt = Pt
-- em = Em
cm = Cm
mm = Mm
ex = Ex
pc = Pc
inch = In
sp = Sp
bp = Bp
dd = Dd
cc = Cc
mu = Mu

-- simulate the <hr> html tag
hr :: LatexItem
hr = group $ noindent <> rule linewidth (pt 1.5)

normSpaces :: String -> String
normSpaces = unlines . map (L.unwords . words) . lines

num :: Real a => a -> LatexItem
num = size . SizeRat . toRational

rat :: Rational -> LatexItem
rat = size . SizeRat

hstring :: String -> LatexItem
hstring = fromString


tableofcontents :: ParItem
tableofcontents = parCmdArgs "tableofcontents" []

maketitle :: ParItem
maketitle = parCmdArgs "maketitle" []

-- par = texCmdNoArg "par"
noindent :: LatexItem
noindent = texCmdNoArg "noindent"

-- robust
stretch :: Rational -> LatexSize
stretch = SizeCmdRatArg "stretch"

parindent, textwidth, linewidth, textheight, parsep, parskip, baselineskip, baselinestrech,
  fill, columnsep, columnseprule, mathindent, oddsidemargin, evensidemargin, marginparwidth,
  marginparsep, marginparpush, topmargin, headheight, headsep, topskip, footheight, footskip,
  topsep, partopsep, itemsep, itemindent, labelsep, labelwidth, leftmargin, rightmargin,
  listparindent, jot, abovedisplayskip, belowdisplayskip, abovedisplayshortskip,
  belowdisplayshortskip, floatsep, textfloatsep, intextsep, dblfloatsep, dbltextfloatsep,
  textfraction, floatpagefraction, dbltopfaction, dblfloatpagefraction, arraycolsep,
  tabcolsep, arrayrulewidth, doublerulesep, arraystretch, bigskipamount, medskipamount,
  smallskipamount, fboxrule, fboxsep :: LatexSize

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

reversemarginpar :: TexDecl
reversemarginpar = texDecl "reversemarginpar"
normalmarginpar :: TexDecl
normalmarginpar = texDecl "normalmarginpar"

-- The tabbing Environment

-- TODO

-- Spaces

-- robust
hspace :: LatexSize -> LatexItem
hspace = latexCmdArg "hspace" . size

-- robust
hspaceStar :: LatexSize -> LatexItem
hspaceStar = latexCmdArg "hspace*" . size

-- fragile
-- the says that's a command however putting braces around disable
-- its effect. We expose it as a ParItem since this is its main usage.
vspace :: LatexSize -> ParItem
vspace = parCmdArg "vspace" . size

-- fragile
vspaceStar :: LatexSize -> ParItem
vspaceStar = parCmdArg "vspace*" . size

vfill :: ParItem
vfill = parCmdArgs "vfill" [] -- = vspace fill
hfill :: LatexItem
hfill = texCmdNoArg "hfill" -- = hspace fill
dotfill :: LatexItem
dotfill = texCmdNoArg "dotfill"
hrulefill :: LatexItem
hrulefill = texCmdNoArg "hrulefill"

thinspace :: LatexItem
thinspace = texCmdNoArg "thinspace"

negthinspace :: LatexItem
negthinspace = texCmdNoArg "!"

-- The italic correction space (\/ in LaTeX)
corrspace :: LatexItem
corrspace = texCmdNoArg "/"

-- fragile
bigskip :: ParItem
bigskip = parCmdArgs "bigskip" [] -- = vspace bigskipamount

-- fragile
medskip :: ParItem
medskip = parCmdArgs "medskip" [] -- = vspace medskipamount

-- fragile
smallskip :: ParItem
smallskip = parCmdArgs "smallskip" [] -- = vspace smallskipamount

addvspace :: LatexSize -> ParItem
addvspace = parCmdArg "addvspace" . size


-- Font sizes

-- those could be seen as taking an argument
tiny, scriptsize, footnotesize, small, normalsize, large,
  _LARGE, _Large, huge, _Huge, mdseries, ssfamily :: TexDecl
tiny         = texDecl "tiny"
scriptsize   = texDecl "scriptsize"
footnotesize = texDecl "footnotesize"
small        = texDecl "small"
normalsize   = texDecl "normalsize"
large        = texDecl "large"
_LARGE       = texDecl "LARGE"
_Large       = texDecl "Large"
huge         = texDecl "huge"
_Huge        = texDecl "Huge"

mdseries = texDecl "mdseries"
ssfamily = texDecl "ssfamily"

emph, textrm, textsf, texttt, textmd, textbf,
  textup, textit, textsl, textsc, textnormal :: LatexItem -> LatexItem
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
linebreak, nolinebreak :: Int -> TexDecl
linebreak = texDeclOpt "linebreak" . num
nolinebreak = texDeclOpt "nolinebreak" . num

-- fragile
newline :: LatexItem
newline = texCmdNoArg "newline"
newline' :: LatexSize -> TexDecl
newline' = texDeclOpt "newline" . size

-- robust
hyphen :: LatexItem
hyphen = rawTex "{\\-}" -- check if {...} does not cause trouble here

-- robust
hyphenation :: [String] -> ParItem
hyphenation = parCmdArg "hyphenation" . rawTex . L.unwords -- rawTex is a bit rough here

sloppy, fussy :: TexDecl
sloppy = texDecl "sloppy"
fussy = texDecl "fussy"


sloppypar :: ParItem -> ParItem
sloppypar = parEnvironmentPar "sloppypar" []

-- fragile
pagebreak, nopagebreak :: Int -> TexDecl
(pagebreak, nopagebreak) =
  ((texDeclOpt "pagebreak" =<<) . check0to4 "pagebreak"
  ,(texDeclOpt "nopagebreak" =<<) . check0to4 "nopagebreak")
  where check0to4 s n | n >= 0 && n <= 4 = return $ num n
                      | otherwise        = throwError $ s ++ ": option must be between 0 and 4 not " ++ show i

-- fragile
samepage :: TexDecl
samepage = texDecl "samepage"

-- robust
newpage :: ParItem
newpage = parCmdArgs "newpage" []
-- robust
clearpage :: ParItem
clearpage = parCmdArgs "clearpage" []
-- fragile
cleardoublepage :: ParItem
cleardoublepage = parCmdArgs "cleardoublepage" []

--- Boxes

{-
class Mbox a where
  -- robust
  mbox :: LatexItem -> a

  -- fragile
  makebox :: LatexSize -> LatexSize -> LatexItem -> a

instance Mbox MathItem where
  mbox = MathToLR Nothing Nothing
  makebox = mmakebox

instance Mbox LatexItem where
  mbox = id

instance Mbox ParItem where
-}

-- robust
mbox :: LatexItem -> LatexItem
mbox = latexCmdArg "mbox"

-- fragile
makebox :: LatexSize -> Pos -> LatexItem -> LatexItem
makebox width pos txt =
  latexCmdArgs "makebox" [optional $ size width
                         ,optional $ rawTex [charPos pos]
                         ,mandatory txt]

-- robust
fbox :: LatexItem -> LatexItem
fbox = latexCmdArg "fbox"

-- fragile
framebox :: LatexSize -> Pos -> LatexItem -> LatexItem
framebox width pos txt = latexCmdArgs "framebox" [optional $ size width
                                                 ,optional $ rawTex [charPos pos]
                                                 ,mandatory txt]

-- TODO: make a safe version using a monad
-- fragile
unsafeNewsavebox :: Int -> (SaveBin, LatexItem)
unsafeNewsavebox n =
  let bin = UnsafeMakeSaveBin n
  in (bin, latexCmdArg "newsavebox" $ latexSaveBin bin)

-- robust
sbox :: SaveBin -> LatexItem -> LatexItem
sbox bin txt = latexCmdArgs "sbox" [mandatory $ latexSaveBin bin, mandatory txt]

-- fragile
savebox :: SaveBin -> Maybe LatexSize -> Maybe (Either () ()) -> LatexItem -> LatexItem
savebox bin width dir txt =
  latexCmdArgs "savebox" [mandatory $ latexSaveBin bin
                         ,maybe noArg (optional . size) width
                         ,maybe noArg (optional . either ll rr) dir
                         ,mandatory txt]
  where ll _ = rawTex "l"
        rr _ = rawTex "r"

-- robust
usebox :: SaveBin -> LatexItem
usebox bin = latexCmdArgs "usebox" [mandatory $ latexSaveBin bin]

-- fragile
parbox :: LatexSize -> LatexItem -> LatexItem
parbox width txt =
  latexCmdArgs "parbox" [mandatory $ size width, mandatory  txt]

-- fragile
parboxTop :: LatexSize -> LatexItem -> LatexItem
parboxTop width txt =
  latexCmdArgs "parbox" [optional $ rawTex "t", mandatory $ size width, mandatory  txt]

-- fragile
parboxBot :: LatexSize -> LatexItem -> LatexItem
parboxBot width txt =
  latexCmdArgs "parbox" [optional $ rawTex "b", mandatory $ size width, mandatory  txt]

minipage :: LatexSize -> ParItem -> LatexItem
minipage width txt =
  latexCmdArgs "minipage" [mandatory $ size width, mandatory $ liftM LatexParMode txt]

minipageTop :: LatexSize -> LatexItem -> LatexItem
minipageTop width txt =
  latexCmdArgs "minipage" [optional $ rawTex "t", mandatory $ size width, mandatory  txt]

minipageBot :: LatexSize -> LatexItem -> LatexItem
minipageBot width txt =
  latexCmdArgs "minipage" [optional $ rawTex "b", mandatory $ size width, mandatory  txt]

-- fragile
rule :: LatexSize -> LatexSize -> LatexItem
rule width height = latexCmdArgs "rule" [mandatory $ size width,mandatory $ size height]

-- fragile
rule' :: LatexSize -> LatexSize -> LatexSize -> LatexItem
rule' raise_len width height = latexCmdArgs "rule" [optional $ size raise_len
                                                   ,mandatory $ size width,mandatory $ size height]

-- fragile
raisebox :: LatexSize -> LatexItem -> LatexItem
raisebox raise_len txt =
  latexCmdArgs "raisebox" [mandatory $ size raise_len,mandatory txt]

-- fragile
raisebox' :: LatexSize -> LatexSize -> LatexSize -> LatexItem -> LatexItem
raisebox' raise_len height depth txt =
  latexCmdArgs "raisebox" [mandatory $ size raise_len
                          ,optional $ size height,optional $ size depth,mandatory  txt]

footnote :: LatexItem -> LatexItem
footnote = latexCmdArg "footnote"

caption :: LatexItem -> LatexItem
caption txt = latexCmdArgs "caption" [mandatory txt]
caption' :: String -> LatexItem -> LatexItem
caption' lstentry txt = latexCmdArgs "caption" [optional $ checkentry lstentry, mandatory txt]
  where checkentry x
          | all isAlphaNum x = rawTex x
          | otherwise        = throwError "caption': restriction to alphanumeric characters for the lstentry"

label :: Key -> LatexItem
label = latexCmdArg "label" . latexKey
ref :: Key -> LatexItem
ref = latexCmdArg "ref" . latexKey
pageref :: Key -> LatexItem
pageref = latexCmdArg "pageref" . latexKey

-- fragile
cite :: [Key] -> LatexItem
cite = latexCmdArg "cite" . latexKeys
cite' :: LatexItem -> [Key] -> LatexItem
cite' txt keys = latexCmdArgs "cite" [optional txt, mandatory $ latexKeys keys]

-- fragile
nocite :: [Key] -> LatexItem
nocite = latexCmdArg "nocite" . latexKeys

-- sectioning

-- Sectioning commands arguments are 'moving'.
sectioning :: String -> ((LatexItem -> ParItem), (Star -> Maybe LatexItem -> LatexItem -> ParItem))
sectioning name = (sect, sect')
  where sect = sect' NoStar Nothing
        sect' star opt arg = parCmdArgs (name ++ addstar star)
                                        (maybeToList (fmap optional opt) ++ [mandatory arg])
        addstar Star   = "*"
        addstar NoStar = ""

part, chapter, section, subsection,  subsubsection, paragraph,
  subparagraph :: LatexItem -> ParItem
part', chapter', section', subsection', subsubsection', paragraph',
  subparagraph' :: Star -> Maybe LatexItem -> LatexItem -> ParItem

(part, part')       = sectioning "part"
(chapter, chapter') = sectioning "chapter"
(section, section') = sectioning "section"
(subsection, subsection')       = sectioning "subsection"
(subsubsection, subsubsection') = sectioning "subsubsection"
(paragraph, paragraph')         = sectioning "paragraph"
(subparagraph, subparagraph')   = sectioning "subparagraph"

-- | Don't confuse 'paragraph' with 'para', 'para' is to make a paragraph,
-- 'paragraph' is to group a set of paragraphs.
para :: LatexItem -> ParItem
para = liftM Para

bibliography :: LatexItem -> ParItem
bibliography = parCmdArg "bibliography"
bibliographystyle :: LatexItem -> ParItem
bibliographystyle = parCmdArg "bibliographystyle"
thispagestyle :: LatexItem -> ParItem
thispagestyle = parCmdArg "thispagestyle"

appendix :: ParItem
appendix = parCmdArgs "appendix" []

-- should be \setlength{a}{b}{c} ...
setlength :: LatexItem -> LatexItem
setlength = latexCmdArg "setlength"

item :: ParItem -> ListItem
item = liftM $ ListItm Nothing

item' :: LatexItem -> ParItem -> ListItem
item' a b = liftM2 ListItm (Just <$> a) b

itemize :: [ListItem] -> ParItem
itemize = listLikeEnv "itemize"
enumerate :: [ListItem] -> ParItem
enumerate = listLikeEnv "enumerate"
description :: [ListItem] -> ParItem
description = listLikeEnv "description"

figure, figureStar, table, tableStar :: [LocSpec] -> ParItem -> ParItem
figure = figureLike "figure"
figureStar = figureLike "figure*"
table = figureLike "table"
tableStar = figureLike "table*"

-- Accents

-- | Add a cedila to a letter (\c{...} in LaTeX)
cedil :: LatexItem -> LatexItem
cedil = latexCmdArg "c"

grave :: LatexItem -> LatexItem
grave = latexCmdArg "`"

acute :: LatexItem -> LatexItem
acute = latexCmdArg "'"

uml :: LatexItem -> LatexItem
uml = latexCmdArg "\""

circ :: LatexItem -> LatexItem
circ = latexCmdArg "^"

-- alias of 'circ'
hat :: LatexItem -> LatexItem
hat = latexCmdArg "^"

check :: LatexItem -> LatexItem
check = latexCmdArg "v"

i :: LatexItem
i = texCmdNoArg "i"

j :: LatexItem
j = texCmdNoArg "j"

tilde :: LatexItem -> LatexItem
tilde = latexCmdArg "~"

dot :: LatexItem -> LatexItem
dot = latexCmdArg "d"

ring :: LatexItem -> LatexItem
ring = latexCmdArg "r"

-- TODO find a name
_H :: LatexItem -> LatexItem
_H = latexCmdArg "H"

-- Produces a dot accent over the following (\.{x} in LaTeX)
overdot :: LatexItem -> LatexItem
overdot = latexCmdArg "."

-- Produces a macron (overbar) accent over the following
overbar :: LatexItem -> LatexItem
overbar = latexCmdArg "="

-- Produces a macron (overbar) accent over the following
underbar :: LatexItem -> LatexItem
underbar = latexCmdArg "b"

-- Produces a tie-after accent, as in `oo[' (\t in LaTeX).
tieafter :: LatexItem -> LatexItem
tieafter = latexCmdArg "t"

-- Some non-English characters

-- The 'å' letter (like @ring "a"@)
aa :: LatexItem
aa = texCmdNoArg "aa"

-- The 'Å' letter (like @ring "A"@)
_AA :: LatexItem
_AA = texCmdNoArg "AA"

-- The 'æ' letter
ae :: LatexItem
ae = texCmdNoArg "ae"

-- The 'Æ' letter
_AE :: LatexItem
_AE = texCmdNoArg "AE"

-- The 'œ' letter
oe :: LatexItem
oe = texCmdNoArg "oe"

-- The 'Œ' letter
_OE :: LatexItem
_OE = texCmdNoArg "OE"

-- The 'ß' letter
ss :: LatexItem
ss = texCmdNoArg "ss"

-- The '/l' letter
_l :: LatexItem
_l = texCmdNoArg "l"

-- The '/L' letter
_L :: LatexItem
_L = texCmdNoArg "L"

-- The 'ø' letter
o :: LatexItem
o = texCmdNoArg "o"

-- The 'Ø' letter
_O :: LatexItem
_O = texCmdNoArg "O"

-- Text symbols

-- | The copyright symbol, ©.
copyright :: LatexItem
copyright = texCmdNoArg "copyright"

-- | The dagger symbol (in text).
dag :: LatexItem
dag = texCmdNoArg "dag"

-- | The double dagger symbol (in text).
ddag :: LatexItem
ddag = texCmdNoArg "ddag"

-- | The LaTeX logo.
_LaTeX :: LatexItem
_LaTeX = texCmdNoArg "LaTeX"

-- | The TeX logo.
_TeX :: LatexItem
_TeX = texCmdNoArg "TeX"

-- | An ellipsis (three dots at the baseline): `...'. This command also works in math mode.
ldots :: LatexItem
ldots = texCmdNoArg "ldots"

-- | Left (opening) quote: `.
lq :: LatexItem
lq = texCmdNoArg "lq"

-- | Right (closing) quote: '.
rq :: LatexItem
rq = texCmdNoArg "rq"

-- | Paragraph sign (pilcrow).
_P :: LatexItem
_P = texCmdNoArg "P"

-- | English pounds sterling.
pounds :: LatexItem
pounds = texCmdNoArg "pounds"

-- | Section symbol.
_S :: LatexItem
_S = texCmdNoArg "S"

textdegree :: LatexItem
textdegree = latexCmdArgs "textdegree" [packageDependency (pkgName "textcomp"), mandatory mempty]

-- check options
titlepage, flushleft, center, boxedminipage, quotation, verse :: ParItem -> ParItem
titlepage = parEnvironmentPar "titlepage" []
flushleft = parEnvironmentPar "flushleft" []
center = parEnvironmentPar "center" []
boxedminipage = parEnvironmentPar "boxedminipage" []
quotation = parEnvironmentPar "quotation" []
verse = parEnvironmentPar "verse" []

quote :: LatexItem -> ParItem
quote = liftM $ ParEnvironmentLR "quote"

unwords :: [LatexItem] -> LatexItem
unwords = mconcat . intersperse space

-- The array and tablular Environments

tabularLike :: ([RowSpec a] -> [Row a] -> b) -> [RowSpec (LatexM a)] -> [Row (LatexM a)] -> LatexM b
tabularLike f specs rows = do
  spcs <- mapM sequenceA specs
  f spcs <$> (checkRows spcs =<< mapM sequenceA rows)

tabular :: [RowSpec LatexItem] -> [Row LatexItem] -> ParItem
tabular = tabularLike Tabular

checkRows :: [RowSpec a] -> [Row a] -> LatexM [Row a]
checkRows specs = mapM checkRow
  where checkRow (Cells cs)
          | cols /= length cs    = err "wrong number of cells" cols "different from" (length cs)
          | otherwise            = pure $ Cells cs
        checkRow Hline           = pure Hline
        checkRow (Cline c1 c2)
          | c1 > cols = err "cline: start column too high" c1 ">" cols
          | c1 < 0    = throwError "tabular: cline: negative start column"
          | c2 > cols = err "cline: end column too high" c2 ">" cols
          | c2 < 0    = throwError "tabular: cline: negative end column"
          | otherwise = pure $ Cline c1 c2
        cols = length $ filter isCol specs
        isCol Rc = True
        isCol Rl = True
        isCol Rr = True
        isCol Rvline = False
        isCol (Rtext _) = False
        err msg x op y = throwError $ L.unwords ["tabular:", msg, "(" ++ show x, op, show y ++ ")"] 

cells :: [a] -> Row a
cells = Cells

cell :: a -> Row a
cell = Cells . (:[])

hline :: Row a
hline = Hline
cline :: Int -> Int -> Row a
cline = Cline

-- this is more the '|' than the \vline of LaTeX,
-- one may want to support both using a HaveVline type class.
vline :: RowSpec a
vline = Rvline

rtext :: a -> RowSpec a
rtext = Rtext

class HaveC a where c :: a
class HaveL a where l :: a
class HaveR a where r :: a

instance HaveC (RowSpec a) where c = Rc
instance HaveL (RowSpec a) where l = Rl
instance HaveR (RowSpec a) where r = Rr

-- eqnarraystar = 

a4paper :: LatexPaper
a4paper = A4paper

book, article, report, letter :: DocumentClass
book = Book
article = Article
report = Report
letter = Letter

documentclass :: Maybe LatexSize -> Maybe LatexPaper -> DocumentClass -> PreambleItem
documentclass msize mpaper dc =
  preambleCmdArgs "documentclass" [optionals (maybeToList (fmap size msize)
                                             ++ maybeToList (fmap (rawTex . showPaper) mpaper))
                                  ,mandatory $ rawTex $ showDocumentClass dc]

{-
$(
 let
  mathCmdsArg, allTexDecls, mathDecls :: [String]
  mathCmds :: [(String, String)]

  mathCmds =
    [("lbrace", "{")
    ,("rbrace", "}")
    ,("space", " ")
    ,("at", "@")
    ,("in_", "in")
    ,("forall_", "forall")
    ,("thinspace", ",")
    ,("negthinspace", "!")
    ,("mediumspace", ":")
    ,("thickspace", ";")
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

  allTexDecls = typeStyles

  mathDecls = ["displaystyle", "textstyle", "scriptstyle", "scriptscriptstyle"
              ,"mit","cal"
              ]

  lowerName :: String -> Name
  lowerName name | isLower (head name) = mkName name
                | otherwise           = mkName $ '_':name

  mkMathCmd (name, cmd) =
    let lname = lowerName name
    in
    [sigD lname [t| MathItem |]
    , valD (varP lname) (normalB [| mathCmd $(stringE cmd) |]) []
    ]

  mkMathCmdArg name =
    let lname = lowerName name in
    [sigD lname [t| MathItem -> MathItem |]
    , valD (varP lname) (normalB [| mathCmdArg $(stringE name) |]) []
    ]

  mkTexDecl name =
    let lname = lowerName name in
    [sigD lname [t| LatexItem |]
    , valD (varP lname) (normalB [| texDecl $(stringE name) [] |]) []
    ]

  mkMathDecl name =
    let lname = lowerName name in
    [sigD lname [t| MathItem |]
     , valD (varP lname) (normalB [| mathDecl $(stringE name) [] |]) []
    ]

  mkList name ty names =
    [sigD name ty
    ,valD (varP name) (normalB (listE $ map (varE . lowerName) names)) []]

  d = sequence $ concat $ concat
      [ map mkMathCmd mathCmds
      , map mkMathCmdArg mathCmdsArg
      , map mkTexDecl allTexDecls
      , map mkMathDecl mathDecls
      , [mkList (mkName "mathItems") [t| [MathItem] |] $ (map fst mathCmds ++ mathDecls)]
      , [mkList (mkName "mathCmdsArg") [t| [MathItem -> MathItem] |] mathCmdsArg]
      , [mkList (mkName "allTexDecls") [t| [LatexItem] |] allTexDecls]
      ]
  in do dd <- d
        runIO $ writeFile "/tmp/a.hs" $ pprint dd
        return []
 )
 -}

{- This chunk was generated by the previous TH splice.
   The lists of all commands are manually maintained though. -}
author, title, subtitle, date, institute :: LatexItem -> PreambleItem
title = preambleCmdArg "title"
subtitle = preambleCmdArg "subtitle"
date = preambleCmdArg "date"
author = preambleCmdArg "author"
institute = preambleCmdArg "institute"

usepackage :: [Arg LatexItem] -> PackageName -> PreambleItem
usepackage args pkg = Usepackage pkg <$> mapM sequenceA args

authors :: [LatexItem] -> PreambleItem
authors = author . mconcat . intersperse (rawTex " & ")


{-# DEPRECATED em "Use emph instead" #-}
em :: TexDecl
em = texDecl "em"
{-# DEPRECATED bf "Use textbf instead" #-}
bf :: TexDecl
bf = texDecl "bf"
{-# DEPRECATED sf "Use textsf instead" #-}
sf :: TexDecl
sf = texDecl "sf"
{-# DEPRECATED sl "Use textsl instead" #-}
sl :: TexDecl
sl = texDecl "sl"
{-# DEPRECATED sc "Use textsc instead" #-}
sc :: TexDecl
sc = texDecl "sc"
{-# DEPRECATED it "Use textit instead" #-}
it :: TexDecl
it = texDecl "it"
{-# DEPRECATED tt "Use texttt instead" #-}
tt :: TexDecl
tt = texDecl "tt"

allTexDecls :: [TexDecl]
allTexDecls = [em, bf, sf, sl, sc, it, tt]

