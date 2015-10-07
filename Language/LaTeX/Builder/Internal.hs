module Language.LaTeX.Builder.Internal where

import Prelude hiding (sqrt, min, max, lcm, gcd, log, mod, tanh, cosh, tan, sinh,
                       sin, cos, succ, sum, pi, mapM)
import qualified Data.List as L
import Data.Maybe
import Data.Monoid
import Data.Traversable (sequenceA, mapM)
import Control.Applicative hiding (optional)
import Control.Monad hiding (mapM)
import Control.Monad.Error (throwError)

import Language.LaTeX.Types
import Language.LaTeX.Builder.MonoidUtils


{- TODO:
    - robust/fragile/moving
    - tracking savebin in the monad?
    - generating a doc with examples:
         [...("sum", [| sum⊕sub(i⊕eq⊕0)⊕sup infty⊕i⊕sup 2 |])...]
    - pictures
    - choose between optFoo and optionalFoo
 -}


{-
  Common machinery for arguments
-}

rawAnyTex :: String -> AnyItem
rawAnyTex = latexItem . rawTex

noArg :: Arg a
noArg = NoArg

starArg :: Arg a
starArg = StarArg

starToArg :: Star -> Arg a
starToArg Star   = starArg
starToArg NoStar = noArg

mandatory, optional :: a -> Arg a
mandatory = Mandatory . pure
optional = Optional . pure

mandatoryLatexItem, optionalLatexItem :: LatexItem -> Arg AnyItem
mandatoryLatexItem = mandatory . latexItem
optionalLatexItem = optional . latexItem

coordinates :: a -> a -> Arg a
coordinates = Coordinates

mandatoryList :: [a] -> Arg a
mandatoryList = Mandatory

optionals :: [a] -> Arg a
optionals [] = NoArg
optionals xs = Optional xs

named :: String -> a -> Named a
named = Named

namedOpts :: [Named a] -> Arg a
namedOpts [] = NoArg
namedOpts xs = NamedOpts xs

namedArgs :: [Named a] -> Arg a
namedArgs = NamedArgs

optionalLatexItems :: [LatexItem] -> Arg AnyItem
optionalLatexItems = optionals . map latexItem

-- Note that @rawArg ø@ reduces to ø.
rawArg :: String -> Arg m
rawArg "" = NoArg
rawArg x  = RawArg x

-- Note that @liftArg ø@ reduces to ø.
liftArg :: (Eq m, Monoid m) => m -> Arg m
liftArg x | x == ø    = NoArg
          | otherwise = LiftArg x

{-
   LaTeX Preamble Items
-}

usepackage :: [AnyItem] -> PackageName -> PreambleItem
usepackage opts pkg =
  preambleCmdArgs "usepackage"
    [providePackage pkg, optionals opts, mandatory (packageName pkg)]

preambleNote :: Key -> Note -> PreambleItem -> PreambleItem
preambleNote k = fmap . PreambleNote k

preambleCmdArgs :: String -> [Arg AnyItem] -> PreambleItem
preambleCmdArgs x ys = PreambleCmdArgs x <$> mapM (mapM anyItmM) ys

preambleCmdArg :: String -> AnyItem -> PreambleItem
preambleCmdArg x y = preambleCmdArgs x [mandatory y]

preambleEnv :: String -> [Arg AnyItem] -> AnyItem -> PreambleItem
preambleEnv x ys = liftM2 (PreambleEnv x) (mapM (mapM anyItmM) ys) . anyItmM

rawPreamble :: String -> PreambleItem
rawPreamble = mapNonEmpty $ pure . RawPreamble

preambleCast :: AnyItem -> PreambleItem
preambleCast = fmap cast . anyItmM
  where cast (PreambleItm x) = x
        cast x               = PreambleCast x

{-
   Notes
-}

nilNote :: Note
nilNote = stringNote ""

stringNote :: String -> Note
stringNote = TextNote

intNote :: Int -> Note
intNote = IntNote

locNote :: Loc -> Note
locNote = LocNote

{-
   TeX Declarations
-}

rawDecls :: [TexDecl] -> LatexItem
rawDecls = mapNonEmpty $ fmap TexDecls . sequenceA

texDecl :: String -> TexDecl
texDecl s = pure $ TexDcl s []

texDecl' :: String -> [Arg AnyItem] -> TexDecl
texDecl' = texDeclArgs
{-# DEPRECATED texDecl' "use texDeclArgs instead" #-}

texDeclArgs :: String -> [Arg AnyItem] -> TexDecl
texDeclArgs s opts = TexDcl s <$> mapM (mapM anyItmM) opts

texDeclArg :: String -> AnyItem -> TexDecl
texDeclArg x y = texDeclArgs x [mandatory y]

texDeclOpt :: String -> AnyItem -> TexDecl
texDeclOpt s opt = texDeclArgs s [optional opt]

-- `{' `}' are like bgroup plus egroup except that `{' and `}' are
-- syntactically forced to be balanced.
-- begingroup and endgroup only save the scopes of definitions.
-- bgroup and egroup save the scopes as well but also resolve the springs
-- independently.
bgroup, egroup, begingroup, endgroup :: TexDecl
bgroup      = texDecl "bgroup"
egroup      = texDecl "egroup"
begingroup  = texDecl "begingroup"
endgroup    = texDecl "endgroup"

{-
   LaTeX commands meant as a “vertical” mode.
-}

parNote :: Key -> Note -> ParItem -> ParItem
parNote k = fmap . ParNote k

parCmdArgs :: String -> [Arg AnyItem] -> ParItem
parCmdArgs x ys = ParCmdArgs x <$> mapM (mapM anyItmM) ys

parCmdArg :: String -> AnyItem -> ParItem
parCmdArg x y = parCmdArgs x [mandatory y]

parItem :: ParItem -> AnyItem
parItem = AnyItem . fmap ParItm

parCast :: AnyItem -> ParItem
parCast = fmap cast . anyItmM
  where cast (ParItm x) = x
        cast x          = ParCast x

parEnv :: String -> [Arg AnyItem] -> AnyItem -> ParItem
parEnv x ys = liftM2 (ParEnv x) (mapM (mapM anyItmM) ys) . anyItmM

parEnvironmentPar :: String -> [Arg AnyItem] -> ParItem -> ParItem
parEnvironmentPar x ys = parEnv x ys . parItem

figureLike :: String -> Star -> [LocSpec] -> ParItem -> ParItem
figureLike name star locs
  = parEnvironmentPar (starize name star)
      [optional . locSpecs $ locs] -- liftM $ FigureLike (starize x s) y

listLikeEnv :: String -> [Arg LatexItem] -> [ListItem] -> ParItem
listLikeEnv name opts items =
  parEnvironmentPar name ((map.fmap) latexItem opts)
                         (mconcat <$> mapM (fmap mkItem) items)
  where mkItem (ListItm opts' contents) = ParCmdArgs "item" ((map.fmap) LatexItm opts') ⊕ contents

{-
  LaTeX commands meant as a “horizontal” mode.
-}

latexNote :: Key -> Note -> LatexItem -> LatexItem
latexNote k = fmap . LatexNote k

latexCmdArgs :: String -> [Arg LatexItem] -> LatexItem
latexCmdArgs x ys = LatexCmdArgs x <$> mapM sequenceA ys

latexCmdAnyArgs :: String -> [Arg AnyItem] -> LatexItem
latexCmdAnyArgs x ys = LatexCmdAnyArgs x <$> mapM (mapM anyItmM) ys

latexCmdArg :: String -> LatexItem -> LatexItem
latexCmdArg x y = latexCmdArgs x [mandatory y]

latexCmdAnyArg :: String -> AnyItem -> LatexItem
latexCmdAnyArg x y = latexCmdAnyArgs x [mandatory y]

latexItem :: LatexItem -> AnyItem
latexItem = AnyItem . fmap LatexItm

latexEnvironmentAny :: String -> [Arg AnyItem] -> AnyItem -> LatexItem
latexEnvironmentAny x ys = liftM2 (Environment x) (mapM (mapM anyItmM) ys) . anyItmM

latexEnvironment :: String -> [Arg AnyItem] -> LatexItem -> LatexItem
latexEnvironment x ys = latexEnvironmentAny x ys . latexItem

space :: LatexItem
space = rawTex "{ }"

rawTex :: String -> LatexItem
rawTex = mapNonEmpty $ pure . RawTex

texCmdNoArg :: String -> LatexItem
texCmdNoArg = pure . TexCmdNoArg

latexCast :: AnyItem -> LatexItem
latexCast = fmap cast . anyItmM
  where cast (LatexItm x) = x
        cast x            = LatexCast x

latexEnvironmentPar :: String -> [Arg AnyItem] -> ParItem -> LatexItem
latexEnvironmentPar x ys = latexEnvironmentAny x ys . parItem

latexParModeArgs :: String -> [Arg AnyItem] -> ParItem -> LatexItem
latexParModeArgs x ys z = latexCmdAnyArgs x (ys ++ [mandatory (parItem z)])

{-
  TeX length commands
-}

texLength :: LatexLength -> AnyItem
texLength = AnyItem . pure . Length

mandatoryTexLength :: LatexLength -> Arg AnyItem
mandatoryTexLength = mandatory . texLength

optTexLength :: LatexLength -> Arg AnyItem
optTexLength = optional . texLength

preambleItem :: PreambleItem -> AnyItem
preambleItem = AnyItem . fmap PreambleItm

packageName :: PackageName -> AnyItem
packageName = AnyItem . pure . PackageName

{-
  Package Names
-}

pkgName :: String -> PackageName
pkgName = PkgName

packageDependency :: PackageName -> Arg a
packageDependency = PackageAction . PackageDependency

-- This phantom argument states that the given package
-- is considered provided from now on.
-- This especially make sense when building the usepackage
-- command.
providePackage :: PackageName -> Arg a
providePackage = PackageAction . ProvidePackage

{-
  Document class, LatexPaperSize
-}

rawEncoding :: String -> Encoding
rawEncoding = Encoding

showPaper :: LatexPaperSize -> String
showPaper A4paper = "a4paper"
showPaper (OtherPaperSize s) = s

latexPaper :: LatexPaperSize -> AnyItem
latexPaper = rawAnyTex . showPaper

otherDocumentClassKind :: String -> DocumentClassKind
otherDocumentClassKind = OtherDocumentClassKind

{-
  Math
-}

mathItem :: MathItem -> AnyItem
mathItem = AnyItem . fmap MathItm . mathItmM

mathCast :: AnyItem -> MathItem
mathCast = MathItem . fmap cast . anyItmM
  where cast (MathItm x) = x
        cast x           = MathCast x

rawMath :: String -> MathItem
rawMath = MathItem . pure . RawMath

rawMathChar :: Char -> MathItem
rawMathChar = rawMath . ('{':) . (:"}")

{-
  Numbers, Rationals, Keys, Boolean values, Coordinates, SaveBin, Location specs
-}

num :: Real a => a -> AnyItem
num = texLength . fromRational . toRational

rat :: Rational -> AnyItem
rat = texLength . fromRational

latexKey :: Key -> AnyItem
latexKey = AnyItem . pure . Key

latexKeys :: [Key] -> [AnyItem]
latexKeys = map latexKey

latexKeysArg :: [Key] -> Arg AnyItem
latexKeysArg = mandatoryList . latexKeys

latexKeyArg :: Key -> Arg AnyItem
latexKeyArg = mandatory . latexKey

bool :: Bool -> AnyItem
bool True  = rawAnyTex "true"
bool False = rawAnyTex "false"

coord :: Coord -> AnyItem
coord = AnyItem . pure . Coord

locSpecs :: [LocSpec] -> AnyItem
locSpecs = AnyItem . pure . LocSpecs

latexSaveBin :: SaveBin -> AnyItem
latexSaveBin = AnyItem . pure . SaveBin

-- TODO: make a safe version using a monad
-- http://www.personal.ceu.hu/tex/spacebox.htm#newsavebox
-- fragile
unsafeNewsavebox :: Int -> (SaveBin, LatexItem)
unsafeNewsavebox n =
  let bin = UnsafeMakeSaveBin n
  in (bin, latexCmdAnyArg "newsavebox" $ latexSaveBin bin)

{-
   Sectioning, tabulars, rows
-}

-- Sectioning commands arguments are 'moving'.
sectioning :: String -> (LatexItem -> ParItem,
                         Star -> Maybe LatexItem -> LatexItem -> ParItem)
sectioning name = (sect, sect')
  where sect = sect' ø Nothing
        sect' s opt arg = parCmdArgs (starize name s)
                                     (maybeToList (fmap (optional . latexItem) opt) ++
                                      [mandatory (latexItem arg)])

-- The array and tablular Environments

tabularLike :: ([RowSpec a] -> [Row a] -> b) -> [RowSpec (LatexM a)] -> [Row (LatexM a)] -> LatexM b
tabularLike f specs rows = do
  spcs <- mapM sequenceA specs
  f spcs <$> (checkRows spcs =<< mapM sequenceA rows)

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
        err msg x op y = throwError $ L.unwords ["tabular:", msg, '(' : show x, op, show y ++ ")"]

{-
  Misc
-}

normSpaces :: String -> String
normSpaces = unlines . map (L.unwords . words) . lines

starize :: String -> Star -> String
starize s NoStar = s
starize s Star   = s ++ "*"
