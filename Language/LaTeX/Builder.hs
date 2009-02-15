module Language.LaTeX.Builder where

import Prelude hiding (sqrt, min, max, lcm, gcd, log, mod, tanh, cosh, tan, sinh,
                       sin, cos, succ, sum, pi, mapM)
import Data.List hiding (sum, and, group)
import Data.Maybe
import Data.Ratio
import Data.Monoid
import Data.Char
import Data.Traversable (sequenceA, mapM)
import Data.String (fromString)
import Control.Applicative hiding (optional)
import Control.Monad hiding (mapM)
import Control.Monad.Error (throwError)
import Control.Monad.Writer (Writer, execWriter, tell)
import Control.Arrow

import Language.LaTeX.Types
import Language.LaTeX.Internal
import Language.LaTeX.Printer (ppSize)

{-
import Prelude (writeFile, id, Monad(..), fst)
import Language.Haskell.TH
-}

class Group a where
  group :: a -> a

instance (Group a) => Group (LatexM a) where
  group = fmap group

instance Group LatexItm where group = TexGroup
instance Group MathItm where group = MathGroup

infixr 7 !<
infixr 7 <!
infixr 7 !<!

(!<) :: Monoid b => (a -> b) -> a -> Writer b ()
(!<) f x = tell $ f x

(<!) :: (a -> b) -> Writer a () -> b
(<!) f m = f $ execWriter m

-- NOTE: This combinator seems pretty promising...
(!<!) :: Monoid b => (a -> b) -> Writer a () -> Writer b ()
(!<!) f m = tell $ f $ execWriter m

mandatory, optional :: a -> Arg a
mandatory x = Arg Mandatory x
optional x = Arg Optional x

mathCmdArgs :: String -> [Arg MathItem] -> MathItem
mathCmdArgs x ys = MathCmdArgs x <$> mapM sequenceA ys

mathCmdArg :: String -> MathItem -> MathItem
mathCmdArg x y = mathCmdArgs x [mandatory y]

mathDecl :: String -> MathItem
mathDecl = pure . MathDecl

texDecl :: String -> LatexItem
texDecl = pure . TexDecl

texDeclOpt :: String -> LatexItem -> LatexItem
texDeclOpt = liftM . TexDeclOpt

parCmdArgs :: String -> [Arg LatexItem] -> ParItem
parCmdArgs x ys = ParCmdArgs x <$> mapM sequenceA ys

parCmdArg :: String -> LatexItem -> ParItem
parCmdArg x y = parCmdArgs x [mandatory y]

latexCmdArgs :: String -> [Arg LatexItem] -> LatexItem
latexCmdArgs x ys = LatexCmdArgs x <$> mapM sequenceA ys

latexCmdArg :: String -> LatexItem -> LatexItem
latexCmdArg x y = latexCmdArgs x [mandatory y]

mathCmd :: String -> MathItem
mathCmd x = pure $ MathCmdArgs x []

mathBinOp :: String -> MathItem -> MathItem -> MathItem
mathBinOp = liftM2 . MathBinOp

rawMath :: String -> MathItem
rawMath = pure . RawMath

mathGroup :: MathItem -> MathItem
mathGroup = liftM MathGroup

latexSize :: LatexSize -> LatexItem
latexSize = pure . LatexSize

latexSaveBin :: SaveBin -> LatexItem
latexSaveBin = pure . LatexSaveBin

parEnvironmentPar :: String -> [Arg LatexItem] -> ParItem -> ParItem
parEnvironmentPar x ys = liftM2 (ParEnvironmentPar x) $ mapM sequenceA ys

parDecl :: String -> ParItem
parDecl = pure . ParDecl

figureLike :: String -> [LocSpec] -> ParItem -> ParItem
figureLike x y = liftM $ FigureLike x y

listLikeEnv :: String -> [ListItem] -> ParItem
listLikeEnv name items =
  parEnvironmentPar name [] (mconcat <$> mapM (fmap mkItem) items)
  where mkItem (ListItm Nothing contents)    = ParDecl "item" <> contents
        mkItem (ListItm (Just lab) contents) = ParDeclOpt "item" lab <> contents

rawTex :: String -> LatexItem
rawTex = pure . RawTex

texCmdNoArg :: String -> LatexItem
texCmdNoArg = pure . TexCmdNoArg

preambleCmdArg :: String -> LatexItem -> PreambleItem
preambleCmdArg = liftM . PreambleCmdArg

latexKey :: Key -> LatexItem
latexKey = pure . LatexKeys . (:[])

latexKeys :: [Key] -> LatexItem
latexKeys = pure . LatexKeys

root :: PreambleItem -> LatexM Document -> LatexM Root
root = liftM2 Root

document :: ParItem -> LatexM Document
document = liftM Document

math :: MathItem -> LatexItem
math = liftM MathInline
displaymath :: MathItem -> ParItem
displaymath = liftM DisplayMath

mathNeedPackage :: String -> MathItem -> MathItem
mathNeedPackage = liftM . MathNeedPackage

mathToLR :: String -> LatexItem -> MathItem
mathToLR = liftM . MathToLR

dash1, dash2, dash3, nbsp :: LatexItem
dash1 = rawTex "{-}"
dash2 = rawTex "{--}"
dash3 = rawTex "{---}"

nbsp = rawTex "{~}"
-- Do we want to treat '\160' as an nbsp too?

mint :: Int -> MathItem
mint = pure . fromIntegral
mrat :: Rational -> MathItem
mrat = pure . fromRational

sub, sup :: MathItem -> MathItem
sub x = rawMath "_" <> mathGroup x
sup x = rawMath "^" <> mathGroup x

frac, stackrel :: MathItem -> MathItem -> MathItem
frac x y = mathCmdArgs "frac" [mandatory x,mandatory y]
stackrel x y = mathCmdArgs "stackrel" [mandatory x,mandatory y]

sqrt :: MathItem -> MathItem
sqrt x = mathCmdArgs "sqrt" [mandatory x]

sqrt' :: MathItem -> MathItem -> MathItem
sqrt' n x = mathCmdArgs "sqrt" [optional n, mandatory x]

mleft, mright :: Char -> MathItem
mleft x  = rawMath "\\left"  <> (RawMath <$> parenChar x)
mright x = rawMath "\\right" <> (RawMath <$> parenChar x)

between :: Char -> Char -> MathItem -> MathItem
between opening closing x = mleft opening <> x <> mright closing

parens, braces, brackets :: MathItem -> MathItem
parens   = between '(' ')'
braces   = between '{' '}'
brackets = between '[' ']'

parenChar :: Char -> LatexM String
parenChar x | x `elem` "([.])" = return [x]
            | x == '{'         = return "\\{"
            | x == '}'         = return "\\}"
            | otherwise        = throwError $ "invalid parenthesis-like: " ++ show x

protect :: String -> LatexItem
protect ""        = mempty
protect ('\n':xs) = newline <> protect xs
protect (' ':xs)  = uncurry (<>) $ (hspace_ . (+1) . length *** protect) $ break (/=' ') xs
  where hspace_ n = hspace $ Em $ 1%2 * fromIntegral n
protect (x:xs)    = uncurry (<>) $ (hstring . (x :) *** protect) $ break (`elem` " \n") xs

href :: LatexItem -> LatexItem -> LatexItem
href x y = latexCmdArgs "href" [mandatory x,mandatory y]
person :: String -> String -> LatexItem
person name email = href (hstring ("mailto:"++email)) (hstring name)

inch, pt, cm, mm, ex, pc :: Rational -> LatexSize
pt = Pt
-- em = Em
cm = Cm
mm = Mm
ex = Ex
pc = Pc
inch = In

-- simulate the <hr> html tag
hr :: LatexItem
hr = group $ noindent <> rule linewidth (pt 1.5)

normSpaces :: String -> String
normSpaces = unlines . map (unwords . words) . lines

hint :: Int -> LatexItem
hint = latexSize . SizeInt . toInteger

hstring :: String -> LatexItem
hstring = fromString

mstring :: String -> MathItem
mstring = fromString

includegraphics = parCmdArgs "includegraphics"

tableofcontents :: ParItem
tableofcontents = parDecl "tableofcontents"

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

reversemarginpar :: LatexItem
reversemarginpar = texDecl "reversemarginpar"
normalmarginpar :: LatexItem
normalmarginpar = texDecl "normalmarginpar"

-- The tabbing Environment

-- TODO

-- Spaces

-- robust
hspace :: LatexSize -> LatexItem
hspace = latexCmdArg "hspace" . latexSize

-- robust
hspaceStar :: LatexSize -> LatexItem
hspaceStar = latexCmdArg "hspace*" . latexSize

-- fragile
vspace :: LatexSize -> LatexItem
vspace = latexCmdArg "vspace" . latexSize

-- fragile
vspaceStar :: LatexSize -> LatexItem
vspaceStar = latexCmdArg "vspace*" . latexSize

vfill :: LatexItem
vfill = texCmdNoArg "vfill" -- = vspace fill
hfill :: LatexItem
hfill = texCmdNoArg "hfill" -- = hspace fill
dotfill :: LatexItem
dotfill = texCmdNoArg "dotfill"
hrulefill :: LatexItem
hrulefill = texCmdNoArg "hrulefill"

-- fragile
bigskip :: LatexItem
bigskip = texCmdNoArg "bigskip" -- = vspace bigskipamount

-- fragile
medskip :: LatexItem
medskip = texCmdNoArg "medskip" -- = vspace medskipamount

-- fragile
smallskip :: LatexItem
smallskip = texCmdNoArg "smallskip" -- = vspace smallskipamount

addvspace :: LatexSize -> LatexItem
addvspace = latexCmdArg "addvspace" . latexSize


-- Font sizes

-- those could be seen as taking an argument
tiny, scriptsize, footnotesize, small, normalsize, large,
  _LARGE, _Large, huge, _Huge, mdseries, ssfamily :: LatexItem
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
linebreak, nolinebreak :: Int -> LatexItem
linebreak = texDeclOpt "linebreak" . hint
nolinebreak = texDeclOpt "nolinebreak" . hint

-- fragile
newline :: LatexItem
newline = texCmdNoArg "newline"
newline' :: LatexSize -> LatexItem
newline' = texDeclOpt "newline" . latexSize

-- robust
hyphen :: LatexItem
hyphen = rawTex "{\\-}" -- check if {...} does not cause trouble here

-- robust
hyphenation :: [String] -> ParItem
hyphenation = parCmdArg "hyphenation" . rawTex . unwords -- rawTex is a bit rough here

sloppy, fussy :: LatexItem
sloppy = texDecl "sloppy"
fussy = texDecl "fussy"


sloppypar :: ParItem -> ParItem
sloppypar = parEnvironmentPar "sloppypar" []

-- fragile
pagebreak, nopagebreak :: Int -> LatexItem
(pagebreak, nopagebreak) =
  ((texDeclOpt "pagebreak" =<<) . check0to4 "pagebreak"
  ,(texDeclOpt "nopagebreak" =<<) . check0to4 "nopagebreak")
  where check0to4 s i | i >= 0 && i <= 4 = return $ hint i
                      | otherwise        = throwError $ s ++ ": option must be between 0 and 4 not " ++ show i

-- fragile
samepage :: LatexItem
samepage = texDecl "samepage"

-- robust
newpage :: ParItem
newpage = parDecl "newpage"
-- robust
clearpage :: ParItem
clearpage = parDecl "clearpage"
-- fragile
cleardoublepage :: ParItem
cleardoublepage = parDecl "cleardoublepage"

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

text :: LatexItem -> MathItem
text = mathNeedPackage "amsmath" . mathToLR "text"

-- robust
mbox :: LatexItem -> LatexItem
mbox = latexCmdArg "mbox"

-- fragile
makebox :: LatexSize -> LatexItem -> LatexItem
makebox width txt = latexCmdArgs "makebox" [optional $ latexSize width,mandatory txt]

-- fragile
makeboxLeft :: LatexSize -> LatexItem -> LatexItem
makeboxLeft width txt =
  latexCmdArgs "makebox" [optional $ latexSize width,optional $ rawTex "l",mandatory txt]

-- fragile
makeboxRight :: LatexSize -> LatexItem -> LatexItem
makeboxRight width txt =
  latexCmdArgs "makebox" [optional $ latexSize width,optional $ rawTex "r",mandatory txt]

-- robust
fbox :: LatexItem -> LatexItem
fbox = latexCmdArg "fbox"

-- fragile
framebox :: LatexSize -> LatexItem -> LatexItem
framebox width txt = latexCmdArgs "framebox" [optional $ latexSize width,mandatory txt]

-- fragile
frameboxLeft :: LatexSize -> LatexItem -> LatexItem
frameboxLeft width txt =
  latexCmdArgs "framebox" [optional $ latexSize width,optional $ rawTex "l",mandatory txt]

-- fragile
frameboxRight :: LatexSize -> LatexItem -> LatexItem
frameboxRight width txt =
  latexCmdArgs "framebox" [optional $ latexSize width,optional $ rawTex "r",mandatory txt]

-- TODO: make a safe version using a monad
-- fragile
unsafeNewsavebox :: Int -> LatexItem
unsafeNewsavebox i =
  let bin = UnsafeMakeSaveBin i
  in latexCmdArg "newsavebox" $ latexSaveBin bin

-- robust
sbox :: SaveBin -> LatexItem -> LatexItem
sbox bin txt = latexCmdArgs "sbox" [mandatory $ latexSaveBin bin, mandatory txt]

-- fragile
savebox :: SaveBin -> LatexSize -> LatexItem -> LatexItem
savebox bin width txt =
  latexCmdArgs "savebox" [mandatory $ latexSaveBin bin, optional $ latexSize width,
                          mandatory  txt]

-- fragile
saveboxLeft :: SaveBin -> LatexSize -> LatexItem -> LatexItem
saveboxLeft bin width txt =
  latexCmdArgs "savebox" [mandatory $ latexSaveBin bin, optional $ latexSize width,
                          optional $ rawTex "l", mandatory  txt]

-- fragile
saveboxRight :: SaveBin -> LatexSize -> LatexItem -> LatexItem
saveboxRight bin width txt =
  latexCmdArgs "savebox" [mandatory $ latexSaveBin bin, optional $ latexSize width,
                          optional $ rawTex "r", mandatory  txt]

-- robust
usebox :: SaveBin -> LatexItem
usebox bin = latexCmdArgs "usebox" [mandatory $ latexSaveBin bin]

-- fragile
parbox :: LatexSize -> LatexItem -> LatexItem
parbox width txt =
  latexCmdArgs "parbox" [mandatory $ latexSize width, mandatory  txt]

-- fragile
parboxTop :: LatexSize -> LatexItem -> LatexItem
parboxTop width txt =
  latexCmdArgs "parbox" [optional $ rawTex "t", mandatory $ latexSize width, mandatory  txt]

-- fragile
parboxBot :: LatexSize -> LatexItem -> LatexItem
parboxBot width txt =
  latexCmdArgs "parbox" [optional $ rawTex "b", mandatory $ latexSize width, mandatory  txt]

minipage :: LatexSize -> ParItem -> LatexItem
minipage width txt =
  latexCmdArgs "minipage" [mandatory $ latexSize width, mandatory $ liftM LatexParMode txt]

minipageTop :: LatexSize -> LatexItem -> LatexItem
minipageTop width txt =
  latexCmdArgs "minipage" [optional $ rawTex "t", mandatory $ latexSize width, mandatory  txt]

minipageBot :: LatexSize -> LatexItem -> LatexItem
minipageBot width txt =
  latexCmdArgs "minipage" [optional $ rawTex "b", mandatory $ latexSize width, mandatory  txt]

-- fragile
rule :: LatexSize -> LatexSize -> LatexItem
rule width height = latexCmdArgs "rule" [mandatory $ latexSize width,mandatory $ latexSize height]

-- fragile
rule' :: LatexSize -> LatexSize -> LatexSize -> LatexItem
rule' raise_len width height = latexCmdArgs "rule" [optional $ latexSize raise_len
                                                   ,mandatory $ latexSize width,mandatory $ latexSize height]

-- fragile
raisebox :: LatexSize -> LatexItem -> LatexItem
raisebox raise_len txt =
  latexCmdArgs "raisebox" [mandatory $ latexSize raise_len,mandatory txt]

-- fragile
raisebox' :: LatexSize -> LatexSize -> LatexSize -> LatexItem -> LatexItem
raisebox' raise_len height depth txt =
  latexCmdArgs "raisebox" [mandatory $ latexSize raise_len
                          ,optional $ latexSize height,optional $ latexSize depth,mandatory  txt]

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

bibliography :: LatexItem -> LatexItem
bibliography = latexCmdArg "bibliography"
bibliographystyle :: LatexItem -> LatexItem
bibliographystyle = latexCmdArg "bibliographystyle"
thispagestyle :: LatexItem -> LatexItem
thispagestyle = latexCmdArg "thispagestyle"

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

-- check options
titlepage, flushleft, boxedminipage, quotation, verse :: ParItem -> ParItem
titlepage = parEnvironmentPar "titlepage" []
flushleft = parEnvironmentPar "flushleft" []
boxedminipage = parEnvironmentPar "boxedminipage" []
quotation = parEnvironmentPar "quotation" []
verse = parEnvironmentPar "verse" []

quote :: LatexItem -> ParItem
quote = liftM $ ParEnvironmentLR "quote"

-- The array and tablular Environments

tabular :: [RowSpec] -> [Row LatexItem] -> ParItem
tabular specs rows = Tabular specs <$> (checkRows specs =<< mapM sequenceA rows)

array :: [RowSpec] -> [Row MathItem] -> MathItem
array specs rows = MathArray specs <$> (checkRows specs =<< mapM sequenceA rows)

checkRows :: [RowSpec] -> [Row a] -> LatexM [Row a]
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
        isCol s = s `elem` [Rc,Rl,Rr]
        err msg x op y = throwError $ unwords ["tabular:", msg, "(" ++ show x, op, show y ++ ")"] 

cells :: [a] -> Row a
cells = Cells
hline :: Row a
hline = Hline
cline :: Int -> Int -> Row a
cline = Cline

-- this is more the '|' than the \vline of LaTeX,
-- one may want to support both using a HaveVline type class.
vline :: RowSpec
vline = Rvline

class HaveC a where c :: a
class HaveL a where l :: a
class HaveR a where r :: a

instance HaveC RowSpec where c = Rc
instance HaveL RowSpec where l = Rl
instance HaveR RowSpec where r = Rr

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
  return $
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
      , map mkTexDecl texDecls
      , map mkMathDecl mathDecls
      , [mkList (mkName "mathItems") [t| [MathItem] |] $ (map fst mathCmds ++ mathDecls)]
      , [mkList (mkName "mathCmdsArg") [t| [MathItem -> MathItem] |] mathCmdsArg]
      , [mkList (mkName "texDecls") [t| [LatexItem] |] texDecls]
      ]
  in do dd <- d
        runIO $ writeFile "/tmp/a.hs" $ pprint dd
        return []
 )
 -}

{- This chunk was generated by the previous TH splice.
   The lists of all commands are manually maintained though. -}
author, usepackage, title, subtitle, date, institute :: LatexItem -> PreambleItem
usepackage = preambleCmdArg "usepackage"
title = preambleCmdArg "title"
subtitle = preambleCmdArg "subtitle"
date = preambleCmdArg "date"
author = preambleCmdArg "author"
institute = preambleCmdArg "institute"

authors :: [LatexItem] -> PreambleItem
authors = author . mconcat . intersperse (rawTex " & ")


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
em :: LatexItem
em = texDecl "em"
{-# DEPRECATED bf "Use textbf instead" #-}
bf :: LatexItem
bf = texDecl "bf"
{-# DEPRECATED sf "Use textsf instead" #-}
sf :: LatexItem
sf = texDecl "sf"
{-# DEPRECATED sl "Use textsl instead" #-}
sl :: LatexItem
sl = texDecl "sl"
{-# DEPRECATED sc "Use textsc instead" #-}
sc :: LatexItem
sc = texDecl "sc"
{-# DEPRECATED it "Use textit instead" #-}
it :: LatexItem
it = texDecl "it"
{-# DEPRECATED tt "Use texttt instead" #-}
tt :: LatexItem
tt = texDecl "tt"
displaystyle :: MathItem
displaystyle = mathDecl "displaystyle"
textstyle :: MathItem
textstyle = mathDecl "textstyle"
scriptstyle :: MathItem
scriptstyle = mathDecl "scriptstyle"
scriptscriptstyle :: MathItem
scriptscriptstyle = mathDecl "scriptscriptstyle"
mit :: MathItem
mit = mathDecl "mit"
cal :: MathItem
cal = mathDecl "cal"
eq :: MathItem
eq = rawMath "{=}"

bmod :: MathItem -> MathItem -> MathItem
bmod = mathBinOp "bmod"

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
               ,negate
               , sqrt
               ]

mathBinOps :: [MathItem -> MathItem -> MathItem]
mathBinOps = [(+),(-),(*),bmod]

texDecls :: [LatexItem]
texDecls = [em, bf, sf, sl, sc, it, tt]

-- beamer
-- alert
-- AtBeginSubsection, AtBeginSection
only :: LatexItem -> LatexItem
only = latexCmdArg "only"

usetheme, usefonttheme :: LatexItem -> PreambleItem
usetheme = preambleCmdArg "usetheme"
usefonttheme = preambleCmdArg "usefonttheme"
