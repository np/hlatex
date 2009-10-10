module Language.LaTeX.Builder
  ( (!$), (!$?), ($?), HaveC(..), HaveL(..), HaveR(..),
Spaces(..), _AA, _AE, _H, _Huge, _L, _LARGE, _LaTeX,
_Large, _O, _OE, _P, _S, _TeX, _l, a4paper, aa, abovedisplayshortskip,
abovedisplayskip, acute, addvspace, ae, allTexDecls, appendix, arraycolsep,
arrayrulewidth, arraystretch, article, author, authors, baselineskip,
baselinestrech, belowdisplayshortskip, belowdisplayskip, bf, bfseries,
bibliography, bibliographystyle, bigskip, bigskipamount, book, boxedminipage,
bp, caption, caption', cc, cedil, cell, cells, center, chapter, chapter',
chapterNoTOC, check, circ, cite, cite', cleardoublepage, clearpage, cline, cm,
columnsep, columnseprule, compressSpaces, copyright, corrspace, dag, dash1,
dash2, dash3, date, dblfloatpagefraction, dblfloatsep, dbltextfloatsep,
dbltopfaction, dd, ddag, decl, decls, description, description', displaymath,
document, documentclass, dot, dotfill, doublerulesep, em, emph, enumerate,
enumerate', evensidemargin, ex, fbox, fboxrule, fboxsep, figure, figureStar,
fill, floatpagefraction, floatsep, flushleft, footheight, footnote,
footnotesize, footskip, framebox, fussy, grave, group, hat, headheight,
headsep, hfill, hline, hr, href, hrulefill, space, hspace, hspaceStar, hspaces,
hstring, huge, hyphen, hyphenation, i, inch, institute, intextsep, it, item,
item', itemindent, itemize, itemize', itemsep, itshape, j, jot, label,
labelsep, labelwidth, large, ldots, leftmargin, letter, linebreak, linewidth,
listparindent, lq, makebox, maketitle, marginparpush, marginparsep,
marginparwidth, math, mathindent, mbox, mdseries, medskip, medskipamount,
minipage, minipageBot, minipageTop, mm, mu, nbsp, negthinspace, newline,
newline', newpage, nocite, noindent, nolinebreak, nopagebreak, normalfont,
normalmarginpar, normalsize, num, o, oddsidemargin, oe, overbar, overdot, pagebreak,
pageref, para, paragraph, paragraph', paragraphNoTOC, parbox, parboxBot,
parboxTop, parindent, parsep, parskip, part, part', partNoTOC, partopsep, pc,
person, phantom, pounds, protect, pt, quotation, quote, raisebox, raisebox',
rat, ref, report, reversemarginpar, rightmargin, ring, rm, rmfamily, root, rq,
rtext, rule, rule', samepage, savebox, sbox, sc, scriptsize, scshape, section,
section', sectionNoTOC, sep, setlength, sf, sffamily, sl, sloppy, sloppypar,
slshape, small, smallskip, smallskipamount, sp, ss, stretch, subparagraph,
subparagraph', subparagraphNoTOC, subsection, subsection', subsectionNoTOC,
subsubsection, subsubsection', subsubsectionNoTOC, subtitle, tabcolsep, table,
tableStar, tableofcontents, tabular, textbf, textdegree, textfloatsep,
textfraction, textheight, textit, textmd, textnormal, textrm, textsc, textsf,
textsl, texttt, textup, textwidth, thinspace, thispagestyle, tieafter, tilde,
tiny, title, titlepage, topmargin, topsep, topskip, tt, ttchar, ttfamily, uml,
underbar, unwords, upshape, usebox, verb, verse, vfill, vline,
vspace, vspaceStar,
  )
  where

import Prelude hiding (sqrt, min, max, lcm, gcd, log, mod, tanh, cosh, tan, sinh,
                       sin, cos, succ, sum, pi, mapM, unwords)
import Data.List hiding (sum, and, group, unwords)
import qualified Data.List as L
import Data.Maybe
import Data.Ratio
import Data.Monoid
import Data.Char
import Data.Foldable (foldMap)
import Data.Traversable (sequenceA, mapM)
import Data.String (IsString(..))
import Control.Applicative hiding (optional)
import Control.Monad hiding (mapM)
import Control.Monad.Error (throwError)
import Control.Monad.Writer (Writer, execWriter, tell)
import Control.Arrow

import Language.LaTeX.Types
import Language.LaTeX.Builder.Internal
import Language.LaTeX.Builder.MonoidUtils


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

math :: MathItem -> LatexItem
math = liftM MathInline . mathItmM

displaymath :: MathItem -> ParItem
displaymath = liftM DisplayMath . mathItmM

decls :: [TexDecl] -> LatexItem -> LatexItem
decls ds x = group (rawDecls ds <> x)

decl :: TexDecl -> LatexItem -> LatexItem
decl d = decls [d]

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

{- GHC bug with OverloadedStrings
f "" = 1
f ('a':_) = 2
f _ = 3
-}

newtype Spaces = Spaces { countSpaces :: Int }

hspaces :: Spaces -> LatexItem
hspaces (Spaces n) = mbox . hspace . Em $ 1%2 * fromIntegral n

compressSpaces :: [Char] -> [Either Char Spaces]
compressSpaces [] = []
compressSpaces (' ':xs)
  = uncurry (:) . (Right . Spaces . (+1) . length *** compressSpaces) . span (==' ') $ xs
compressSpaces (x:xs) = Left x : compressSpaces xs

protect :: String -> LatexItem
protect = foldMap (either f hspaces) . compressSpaces
  where f '\n' = newline
        f  x   = rawTex $ hchar x

ttchar :: Char -> String
ttchar ch | isAscii ch && not (isAlphaNum ch) = "{\\char `\\" ++ [ch,'}']
          | otherwise                         = [ch]

verb :: String -> LatexItem
verb = texttt . foldMap (either (rawTex . ttchar) hspaces) . compressSpaces

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

unwords :: [LatexItem] -> LatexItem
unwords = mconcat . intersperse space


-- Fonts
-- some ref used: http://www.cl.cam.ac.uk/~rf10/pstex/latexcommands.htm

-- Font sizes

-- those could be seen as taking an argument
tiny, scriptsize, footnotesize, small, normalsize, large,
  _LARGE, _Large, huge, _Huge, mdseries, bfseries,
  itshape, slshape, scshape, upshape,
  rmfamily, sffamily, ttfamily, normalfont :: TexDecl
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

bfseries = texDecl "bfseries"
mdseries = texDecl "mdseries"
rmfamily = texDecl "rmfamily"
sffamily = texDecl "sffamily"
ttfamily = texDecl "ttfamily"
upshape = texDecl "upshape"
itshape = texDecl "itshape"
slshape = texDecl "slshape"
scshape = texDecl "scshape"
normalfont = texDecl "normalfont"

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
-- ParItem? But then protect and verb would potentially needs it too ....
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

phantom :: LatexItem -> LatexItem
phantom = latexCmdArg "phantom"

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
minipage width = latexEnvironmentPar "minipage" [mandatory $ size width]

minipageTop :: LatexSize -> ParItem -> LatexItem
minipageTop width = latexEnvironmentPar "minipage" [optional $ rawTex "t", mandatory $ size width]

minipageBot :: LatexSize -> ParItem -> LatexItem
minipageBot width = latexEnvironmentPar "minipage" [optional $ rawTex "b", mandatory $ size width]

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

part, chapter, section, subsection,  subsubsection, paragraph, subparagraph,
  partNoTOC, chapterNoTOC, sectionNoTOC, subsectionNoTOC, subsubsectionNoTOC,
  paragraphNoTOC, subparagraphNoTOC :: LatexItem -> ParItem
part', chapter', section', subsection', subsubsection', paragraph',
  subparagraph' :: Maybe LatexItem -> LatexItem -> ParItem

(part,          partNoTOC,          part')          = sectioning "part"
(chapter,       chapterNoTOC,       chapter')       = sectioning "chapter"
(section,       sectionNoTOC,       section')       = sectioning "section"
(subsection,    subsectionNoTOC,    subsection')    = sectioning "subsection"
(subsubsection, subsubsectionNoTOC, subsubsection') = sectioning "subsubsection"
(paragraph,     paragraphNoTOC,     paragraph')     = sectioning "paragraph"
(subparagraph , subparagraphNoTOC,  subparagraph')  = sectioning "subparagraph"

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
item = liftM $ ListItm []

item' :: LatexItem -> ParItem -> ListItem
item' a b = liftM2 ListItm (pure . optional <$> a) b

itemize :: [ListItem] -> ParItem
itemize = listLikeEnv "itemize" []
enumerate :: [ListItem] -> ParItem
enumerate = listLikeEnv "enumerate" []
description :: [ListItem] -> ParItem
description = listLikeEnv "description" []

itemize' :: Maybe LatexItem -> [ListItem] -> ParItem
itemize' = listLikeEnv "itemize" . pure . maybe noArg optional
enumerate' :: Maybe LatexItem -> [ListItem] -> ParItem
enumerate' = listLikeEnv "enumerate" . pure . maybe noArg optional
description' :: Maybe LatexItem -> [ListItem] -> ParItem
description' = listLikeEnv "description" . pure . maybe noArg optional

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

-- The array and tablular Environments

tabular :: [RowSpec LatexItem] -> [Row LatexItem] -> ParItem
tabular = tabularLike Tabular

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

authors :: [LatexItem] -> PreambleItem
authors = author . mconcat . intersperse (rawTex " & ")


{-# DEPRECATED em "Use emph instead" #-}
em :: TexDecl
em = texDecl "em"
{-# DEPRECATED rm "Use textrm or rmfamily instead" #-}
rm :: TexDecl
rm = texDecl "rm"
{-# DEPRECATED bf "Use textbf or bfseries instead" #-}
bf :: TexDecl
bf = texDecl "bf"
{-# DEPRECATED sf "Use textsf or sffamily instead" #-}
sf :: TexDecl
sf = texDecl "sf"
{-# DEPRECATED sl "Use textsl or slshape instead" #-}
sl :: TexDecl
sl = texDecl "sl"
{-# DEPRECATED sc "Use textsc or scshape instead" #-}
sc :: TexDecl
sc = texDecl "sc"
{-# DEPRECATED it "Use textit or itshape instead" #-}
it :: TexDecl
it = texDecl "it"
{-# DEPRECATED tt "Use texttt or ttfamily instead" #-}
tt :: TexDecl
tt = texDecl "tt"
-- \up ?

allTexDecls :: [TexDecl]
allTexDecls = [rm, em, bf, sf, sl, sc, it, tt
              ,rmfamily, bfseries, sffamily, slshape
              ,scshape, itshape, ttfamily, upshape, normalfont
              ,reversemarginpar,normalmarginpar
              ,sloppy, fussy,samepage
              ,tiny, scriptsize, footnotesize, small, normalsize, large
              ,_LARGE, _Large, huge, _Huge]

