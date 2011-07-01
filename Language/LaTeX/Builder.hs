module Language.LaTeX.Builder
  ( (!$), (!$?), ($?), HaveC(..), HaveL(..), HaveR(..),
Spaces(..),
-- * Injecting declarations
decl, decls, parDecls, parDecl,
-- * Others
_AA, _AE, _H, _Huge, _L, _LARGE, _LaTeX,
_Large, _O, _OE, _P, _S, _TeX, _l, XChar, a4paper, aa,
acute, addvspace, ae, allTexDecls, appendix,
article, author, authors,
bf, bfseries, bigskip,
bibliography, bibliographystyle, book, boxedminipage,
caption, caption', cedil, cell, cells, center, chapter, chapter',
check, circ, cite, cite', cleardoublepage, clearpage, cline,
comment, compressSpaces, copyright, corrspace, dag, dash1,
dash2, dash3, date, ddag,
description, displaymath,
document, documentclass, dot, dotfill, em, emph, enumerate,
fbox, figure, flushleft, footnote,
footnotesize, framebox, fussy, grave, group, hat, hchar,
hfill, hline, hr, href, hrulefill, space, hspace, hspace', hspaces,
hstring, huge, hyphen, hyphenation, i, institute, it, item,
item', itemize, itshape, j, label,
large, ldots, letter, linebreak, linebr,
lq, makebox, maketitle,
math, mbox, mdseries, medskip,
minipage, minipageBot, minipageTop, nbsp, negthinspace, newline, rawNewline,
newpage, nocite, noindent, nolinebreak, nopagebreak, normalfont,
normalmarginpar, normalsize, num, o, oe, overbar, overdot, pagebreak,
pageref, pagestyle, para, paragraph, paragraph', parbox, parboxBot,
parboxTop, part, part', person, phantom, pounds,
protect, protector, quotation, quote, raisebox, raisebox',
rat, ref, report, reversemarginpar, ring, rm, rmfamily, rq,
rtext, rule, rule', samepage, savebox, sbox, sc, scriptsize, scshape, section,
section', sep, setlength, addtolength, settowidth, sf, sffamily, sl, sloppy, sloppypar,
slshape, small, smallskip, spaceProtector, ss, subparagraph,
subparagraph', subsection, subsection',
subsubsection, subsubsection', subtitle, table,
tableofcontents, tabular, textbf, textdegree,
textit, textmd, textnormal, textrm, textsc, textsf,
textsl, texttt, textup, thinspace, thispagestyle, tieafter, tilde,
tiny, title, titlepage, tt, ttchar, ttfamily, uml,
underbar, unwords, upshape, usebox, verb, verse, vfill, vline,
vphantom, vspace, vspace', (★), vbox, vtop, hbox, here, top, bottom, page,
centered, flushLeft, flushRight, stretch,
-- * Input Encodings (inputenc package)
utf8,latin1,inputenc,fromEncoding,
  )
  where



import Prelude hiding (sqrt, min, max, lcm, gcd, log, mod, tanh, cosh, tan, sinh,
                       sin, cos, succ, sum, pi, mapM, unwords)
import Data.List hiding (sum, and, group, unwords)
import qualified Data.List as L
import Data.Ratio
import Data.Monoid
import Data.Char
import Data.Maybe
import Data.Foldable (foldMap)
import Data.Traversable
import Data.String (IsString(..))
import Control.Applicative hiding (optional)
import Control.Monad hiding (mapM)
import Control.Monad.Error (throwError)
import Control.Monad.Writer (Writer, execWriter, tell)
import Control.Arrow

import Language.LaTeX.Types
import Language.LaTeX.Builder.Internal
import qualified Language.LaTeX.Length as L
import Language.LaTeX.Builder.MonoidUtils


{-
import Prelude (writeFile, id, Monad(..), fst)
import Language.Haskell.TH
-}


-- references: http://dmr.ath.cx/notes/tex.html

(★) :: Star
(★) = Star

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
decls ds x = group (rawDecls ds ⊕ x)

decl :: TexDecl -> LatexItem -> LatexItem
decl = decls . pure

parDecls :: [TexDecl] -> ParItem
parDecls = para . rawDecls

parDecl :: TexDecl -> ParItem
parDecl = parDecls . pure

document :: DocumentClass -> PreambleItem -> ParItem -> LatexM Document
document = liftM3 Document

dash1, dash2, dash3, nbsp :: LatexItem
dash1 = rawTex "{-}"
dash2 = rawTex "{--}"
dash3 = rawTex "{---}"

nbsp = rawTex "{~}"
-- Do we want to treat '\160' as an nbsp too?

-- | 'sep' is like '{}' in LaTeX, it allows to force letters to be treaten separately,
-- for instance to separate the two 'f's in shelfful.
sep :: LatexItem
sep = group ø

{- GHC bug with OverloadedStrings
f "" = 1
f ('a':_) = 2
f _ = 3
-}

newtype Spaces = Spaces { countSpaces :: Int }

hspaces :: Spaces -> LatexItem
hspaces = mbox . hspace . L.em . (1%2 *) . fromIntegral . countSpaces

compressSpaces :: String -> [Either Char Spaces]
compressSpaces [] = []
compressSpaces (' ':xs)
  = uncurry (:) . (Right . Spaces . (+1) . length *** compressSpaces) . span (==' ') $ xs
compressSpaces (x:xs) = Left x : compressSpaces xs

type XChar = Char -> LatexItem

spaceProtector :: XChar -> String -> LatexItem
spaceProtector xchar = foldMap (either xchar hspaces) . compressSpaces

protector :: XChar -> String -> LatexItem
protector = spaceProtector . nlchar

protect :: String -> LatexItem
protect = protector hchar

-- Turns @'\n'@ into 'newline' and others with the given translator.
nlchar :: XChar -> XChar
nlchar _      '\n'  = rawNewline ø
nlchar xchar  ch    = xchar ch

hchar :: XChar
hchar = rawTex . rawhchar

ttchar :: XChar
ttchar ch | isAscii ch &&
            isPrint ch &&
            not (isAlphaNum ch)  = rawTex $ "{\\char `\\" ++ ch : "}"
          | isAscii ch &&
            not (isPrint ch)     = verb . show $ ch
          | otherwise            = rawTex [ch]

verb :: String -> LatexItem
verb = texttt . protector ttchar

-- A comment put in the generated LaTeX document
comment :: String -> ParItem
comment s = parNote (Key "comment") (stringNote s) ø

href :: LatexItem -> LatexItem -> LatexItem
href x y = latexCmdArgs "href" [mandatory x,mandatory y]

person :: String -> String -> LatexItem
person name email = href (hstring ("mailto:"++email)) (hstring name)

-- simulate the <hr> html tag
hr :: LatexItem
hr = group $ noindent ⊕ rule L.linewidth (L.pt 1.5)

hstring :: String -> LatexItem
hstring = fromString


tableofcontents :: ParItem
tableofcontents = parCmdArgs "tableofcontents" []

maketitle :: ParItem
maketitle = parCmdArgs "maketitle" []

-- par = texCmdNoArg "par"
noindent :: LatexItem
noindent = texCmdNoArg "noindent"

-- Marginal Notes

reversemarginpar :: TexDecl
reversemarginpar = texDecl "reversemarginpar"
normalmarginpar :: TexDecl
normalmarginpar = texDecl "normalmarginpar"

-- The tabbing Environment

-- TODO

-- Spaces

-- robust
-- http://www.personal.ceu.hu/tex/spacebox.htm#hspace
hspace' :: Star -> LatexLength -> LatexItem
hspace' s = latexCmdArg (starize "hspace" s) . texLength

-- robust
-- http://www.personal.ceu.hu/tex/spacebox.htm#hspace
hspace :: LatexLength -> LatexItem
hspace = hspace' ø

-- fragile
-- they says that's a command however putting braces around disable
-- its effect. We expose it as a ParItem since this is its main usage.
-- http://www.personal.ceu.hu/tex/spacebox.htm#vspace
vspace' :: Star -> LatexLength -> ParItem
vspace' s = parCmdArg (starize "vspace" s) . texLength

-- fragile
-- http://www.personal.ceu.hu/tex/spacebox.htm#vspace
vspace :: LatexLength -> ParItem
vspace = vspace' ø

-- http://www.personal.ceu.hu/tex/spacebox.htm#vfill
vfill :: ParItem
vfill = parCmdArgs "vfill" [] -- = vspace fill

-- http://www.personal.ceu.hu/tex/spacebox.htm#hfill
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
-- http://www.personal.ceu.hu/tex/spacebox.htm#bskip
bigskip :: ParItem
bigskip = parCmdArgs "bigskip" [] -- = vspace bigskipamount

-- fragile
-- http://www.personal.ceu.hu/tex/spacebox.htm#bskip
medskip :: ParItem
medskip = parCmdArgs "medskip" [] -- = vspace medskipamount

-- fragile
-- http://www.personal.ceu.hu/tex/spacebox.htm#bskip
smallskip :: ParItem
smallskip = parCmdArgs "smallskip" [] -- = vspace smallskipamount

-- http://www.personal.ceu.hu/tex/spacebox.htm#addvspace
addvspace :: LatexLength -> ParItem
addvspace = parCmdArg "addvspace" . texLength

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
-- http://www.personal.ceu.hu/tex/breaking.htm#linebreak
linebreak :: Int -> TexDecl
linebreak = texDeclOpt "linebreak" . num

-- fragile
-- http://www.personal.ceu.hu/tex/breaking.htm#nolinebreak
nolinebreak :: Int -> TexDecl
nolinebreak = texDeclOpt "nolinebreak" . num

-- http://www.personal.ceu.hu/tex/breaking.htm#linebr
linebr :: Star -> Maybe LatexLength -> LatexItem
linebr s extraSpace =
  latexCmdArgs "\\" [starToArg s
                    ,maybe noArg (optional . texLength) extraSpace
                    ,rawArg "%\n"]

-- fragile
-- http://www.personal.ceu.hu/tex/breaking.htm#newline
rawNewline :: Maybe LatexLength -> LatexItem
rawNewline mlen =
  latexCmdArgs "newline" [maybe noArg (optional . texLength) mlen
                         ,rawArg "%\n"]

-- fragile
-- http://www.personal.ceu.hu/tex/breaking.htm#newline
-- see rawNewline for a more permissive dangerous version of newline
newline :: Maybe LatexLength -> ParItem
newline = para . rawNewline

-- robust
-- http://www.personal.ceu.hu/tex/breaking.htm#hyph
hyphen :: LatexItem
hyphen = rawTex "{\\-}" -- check if {...} does not cause trouble here

-- robust
-- http://www.personal.ceu.hu/tex/breaking.htm#hyphw
hyphenation :: [String] -> ParItem
hyphenation = parCmdArg "hyphenation" . rawTex . L.unwords -- rawTex is a bit rough here

sloppy, fussy :: TexDecl
sloppy = texDecl "sloppy"
fussy = texDecl "fussy"


sloppypar :: ParItem -> ParItem
sloppypar = parEnvironmentPar "sloppypar" []

-- fragile
-- http://www.personal.ceu.hu/tex/breaking.htm#pagebreak
pagebreak :: Int -> TexDecl

-- fragile
-- http://www.personal.ceu.hu/tex/breaking.htm#nopagebreak
nopagebreak :: Int -> TexDecl

(pagebreak, nopagebreak) =
  ((texDeclOpt "pagebreak" =<<) . check0to4 "pagebreak"
  ,(texDeclOpt "nopagebreak" =<<) . check0to4 "nopagebreak")
  where check0to4 s n | n >= 0 && n <= 4 = return $ num n
                      | otherwise        = throwError $ s ++ ": option must be between 0 and 4 not " ++ show i

-- fragile
samepage :: TexDecl
samepage = texDecl "samepage"

-- robust
-- http://www.personal.ceu.hu/tex/breaking.htm#newpage
newpage :: ParItem
newpage = parCmdArgs "newpage" []

-- robust
-- http://www.personal.ceu.hu/tex/breaking.htm#clrpage
clearpage :: ParItem
clearpage = parCmdArgs "clearpage" []

-- fragile
-- http://www.personal.ceu.hu/tex/breaking.htm#clrdblpage
cleardoublepage :: ParItem
cleardoublepage = parCmdArgs "cleardoublepage" []

--- Boxes

-- TeX level
-- http://dmr.ath.cx/notes/tex.html#entry25
hbox :: LatexItem -> LatexItem
hbox = latexCmdArg "hbox"

-- TeX level
-- http://dmr.ath.cx/notes/tex.html#entry25
vbox :: ParItem -> LatexItem
vbox = latexParModeArgs "vbox" []

-- TeX level
-- http://dmr.ath.cx/notes/tex.html#entry25
vtop :: ParItem -> LatexItem
vtop = latexParModeArgs "vtop" []

-- TODO
-- vbox_to :: LatexLength -> ParItem -> LatexItem
-- vbox_to width = latexParModeArgs "vbox" [to $ texLength width]

{-
class Mbox a where
  -- robust
  mbox :: LatexItem -> a

  -- fragile
  makebox :: LatexLength -> LatexLength -> LatexItem -> a

instance Mbox MathItem where
  mbox = MathToLR Nothing Nothing
  makebox = mmakebox

instance Mbox LatexItem where
  mbox = id

instance Mbox ParItem where
-}

-- robust
-- http://www.personal.ceu.hu/tex/spacebox.htm#makebox
mbox :: LatexItem -> LatexItem
mbox = latexCmdArg "mbox"

-- fragile
-- http://www.personal.ceu.hu/tex/spacebox.htm#makebox
makebox :: LatexLength -> Pos -> LatexItem -> LatexItem
makebox width pos txt =
  latexCmdArgs "makebox" [optional $ texLength width
                         ,optional $ rawTex [charPos pos]
                         ,mandatory txt]

-- robust
fbox :: LatexItem -> LatexItem
fbox = latexCmdArg "fbox"

-- fragile
-- http://www.personal.ceu.hu/tex/spacebox.htm#framebox
framebox :: LatexLength -> Pos -> LatexItem -> LatexItem
framebox width pos txt = latexCmdArgs "framebox" [optional $ texLength width
                                                 ,optional $ rawTex [charPos pos]
                                                 ,mandatory txt]

phantom :: LatexItem -> LatexItem
phantom = latexCmdArg "phantom"

vphantom :: LatexItem -> LatexItem
vphantom = latexCmdArg "vphantom"

-- robust
sbox :: SaveBin -> LatexItem -> LatexItem
sbox bin txt = latexCmdArgs "sbox" [mandatory $ latexSaveBin bin, mandatory txt]

-- fragile
savebox :: SaveBin -> Maybe LatexLength -> Maybe (Either () ()) -> LatexItem -> LatexItem
savebox bin width dir txt =
  latexCmdArgs "savebox" [mandatory $ latexSaveBin bin
                         ,maybe noArg (optional . texLength) width
                         ,maybe noArg (optional . either ll rr) dir
                         ,mandatory txt]
  where ll _ = rawTex "l"
        rr _ = rawTex "r"

-- robust
usebox :: SaveBin -> LatexItem
usebox bin = latexCmdArgs "usebox" [mandatory $ latexSaveBin bin]

-- fragile
-- http://www.personal.ceu.hu/tex/spacebox.htm#parbox
parbox :: LatexLength -> LatexItem -> LatexItem
parbox width txt =
  latexCmdArgs "parbox" [mandatory $ texLength width, mandatory  txt]

-- fragile
parboxTop :: LatexLength -> LatexItem -> LatexItem
parboxTop width txt =
  latexCmdArgs "parbox" [optional $ rawTex "t", mandatory $ texLength width, mandatory  txt]

-- fragile
parboxBot :: LatexLength -> LatexItem -> LatexItem
parboxBot width txt =
  latexCmdArgs "parbox" [optional $ rawTex "b", mandatory $ texLength width, mandatory  txt]

minipage :: LatexLength -> ParItem -> LatexItem
minipage width = latexEnvironmentPar "minipage" [mandatory $ texLength width]

minipageTop :: LatexLength -> ParItem -> LatexItem
minipageTop width = latexEnvironmentPar "minipage" [optional $ rawTex "t", mandatory $ texLength width]

minipageBot :: LatexLength -> ParItem -> LatexItem
minipageBot width = latexEnvironmentPar "minipage" [optional $ rawTex "b", mandatory $ texLength width]

-- fragile
-- http://www.personal.ceu.hu/tex/spacebox.htm#rule
rule :: LatexLength -> LatexLength -> LatexItem
rule width height = latexCmdArgs "rule" [mandatory $ texLength width,mandatory $ texLength height]

-- fragile
rule' :: LatexLength -> LatexLength -> LatexLength -> LatexItem
rule' raise_len width height = latexCmdArgs "rule" [optional $ texLength raise_len
                                                   ,mandatory $ texLength width,mandatory $ texLength height]

-- fragile
-- http://www.personal.ceu.hu/tex/spacebox.htm#raisebox
raisebox :: LatexLength -> LatexItem -> LatexItem
raisebox raise_len txt =
  latexCmdArgs "raisebox" [mandatory $ texLength raise_len,mandatory txt]

-- fragile
raisebox' :: LatexLength -> LatexLength -> LatexLength -> LatexItem -> LatexItem
raisebox' raise_len height depth txt =
  latexCmdArgs "raisebox" [mandatory $ texLength raise_len
                          ,optional $ texLength height,optional $ texLength depth,mandatory  txt]

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

part, chapter, section, subsection,  subsubsection, paragraph,
  subparagraph :: LatexItem -> ParItem
part', chapter', section', subsection', subsubsection', paragraph',
  subparagraph' :: Star -> Maybe LatexItem -> LatexItem -> ParItem

(part,          part')          = sectioning "part"
(chapter,       chapter')       = sectioning "chapter"
(section,       section')       = sectioning "section"
(subsection,    subsection')    = sectioning "subsection"
(subsubsection, subsubsection') = sectioning "subsubsection"
(paragraph,     paragraph')     = sectioning "paragraph"
(subparagraph , subparagraph')  = sectioning "subparagraph"

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
pagestyle :: LatexItem -> ParItem
pagestyle = parCmdArg "pagestyle"

appendix :: ParItem
appendix = parCmdArgs "appendix" []

-- http://www.personal.ceu.hu/tex/length.htm
setlength :: LatexLength -> LatexLength -> ParItem
setlength lengthName newLength
  | isJust (lengthCst lengthName)
      = throwError "setlength: the first argument should be a length name not a constant"
  | otherwise
      = parCmdArgs "setlength" [mandatory (texLength lengthName)
                               ,mandatory (texLength newLength)]

-- http://www.personal.ceu.hu/tex/length.htm
addtolength :: LatexLength -> LatexLength -> ParItem
addtolength lengthName newLength
  | isJust (lengthCst lengthName)
      = throwError "addtolength: the first argument should be a length name not a constant"
  | otherwise
      = parCmdArgs "addtolength" [mandatory (texLength lengthName)
                                 ,mandatory (texLength newLength)]

-- http://www.personal.ceu.hu/tex/length.htm
settowidth :: LatexLength -> LatexItem -> ParItem
settowidth lengthName text
  | isJust (lengthCst lengthName)
      = throwError "settowidth: the first argument should be a length name not a constant"
  | otherwise
      = parCmdArgs "settowidth" [mandatory (texLength lengthName)
                                ,mandatory text]

item :: ParItem -> ListItem
item = liftM $ ListItm []

item' :: LatexItem -> ParItem -> ListItem
item' a = liftM2 ListItm (pure . optional <$> a)

itemize :: Maybe LatexItem -> [ListItem] -> ParItem
itemize = listLikeEnv "itemize" . pure . maybe noArg optional

-- enumerate counters are enumi, enumii, enumiii, enumiv
enumerate :: Maybe LatexItem -> [ListItem] -> ParItem
enumerate = listLikeEnv "enumerate" . pure . maybe noArg optional

description :: Maybe LatexItem -> [ListItem] -> ParItem
description = listLikeEnv "description" . pure . maybe noArg optional

figure, table :: Star -> [LocSpec] -> ParItem -> ParItem
figure = figureLike "figure"
table  = figureLike "table"

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
textdegree = latexCmdArgs "textdegree" [packageDependency (pkgName "textcomp"), mandatory ø]

-- check options
titlepage, flushleft, center, boxedminipage, quotation, verse :: ParItem -> ParItem
titlepage = parEnvironmentPar "titlepage" []
flushleft = parEnvironmentPar "flushleft" []
center = parEnvironmentPar "center" []
boxedminipage = parEnvironmentPar "boxedminipage" []
quotation = parEnvironmentPar "quotation" []
verse = parEnvironmentPar "verse" []

quote :: LatexItem -> ParItem
quote = liftM $ ParEnv "quote" [] . LatexItm

-- The array and tablular Environments

tabular :: [RowSpec LatexItem] -> [Row LatexItem] -> ParItem
tabular = tabularLike Tabular

cells :: [a] -> Row a
cells = Cells

cell :: a -> Row a
cell = Cells . pure

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

here, top, bottom, page :: LocSpec
here   = Lh
top    = Lt
bottom = Lb
page   = Lp

centered, flushLeft, flushRight, stretch :: Pos
centered = Centered
flushLeft = FlushLeft
flushRight = FlushRight
stretch = Stretch


-- eqnarraystar = 

a4paper :: LatexPaperSize
a4paper = A4paper

documentclass ::  DocumentClassKind -> [LatexItem] ->
                  DocumentClass
documentclass dc = (DocClass dc <$>) . sequenceA

article ::  Maybe LatexLength -> Maybe LatexPaperSize ->
            [LatexItem] -> DocumentClass
article msize mpaper args =
  documentclass Article $  maybeToList (latexPaper <$> mpaper) ++
                           maybeToList (texLength <$> msize) ++
                           args

-- TODO improve options
letter :: [LatexItem] -> DocumentClass
letter = documentclass Letter

book ::  Maybe LatexLength -> Maybe LatexPaperSize ->
         [LatexItem] -> DocumentClass
book msize mpaper args =
  documentclass Book $  maybeToList (latexPaper <$> mpaper) ++
                        maybeToList (texLength <$> msize) ++
                        args

-- TODO improve options
report :: [LatexItem] -> DocumentClass
report = documentclass Report

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
-- Institute is defined in beamer but not in article.
-- Should we move these definitions into sub modules?
-- For instance Language.LaTeX.Article.date
institute = preambleCmdArg "institute"

authors :: [LatexItem] -> PreambleItem
authors = author . mconcat . intersperse (rawTex " & ")

utf8 :: Encoding
utf8 = rawEncoding "utf8"

latin1 :: Encoding
latin1 = rawEncoding "latin1"

inputenc :: Encoding -> PreambleItem
inputenc (Encoding enc)
  = usepackage [fromString enc] (pkgName "inputenc")

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

