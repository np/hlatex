module Language.LaTeX.Builder.Internal where

import Prelude hiding (sqrt, min, max, lcm, gcd, log, mod, tanh, cosh, tan, sinh,
                       sin, cos, succ, sum, pi, mapM)
import Data.List hiding (sum, and, group)
import qualified Data.List as L
import Data.Maybe
import Data.Ratio
import Data.Monoid
import Data.Char
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
         [...("sum", [| sum<>sub(i<>eq<>0)<>sup infty<>i<>sup 2 |])...]
    - pictures
 -}

noArg :: Arg a
noArg = NoArg

mandatory, optional :: a -> Arg a
mandatory = Mandatory
optional = Optional

coordinates :: a -> a -> Arg a
coordinates = Coordinates

optionals :: [a] -> Arg a
optionals = Optionals

-- Note that @rawArg ""@ reduces to mempty.
rawArg :: String -> Arg a
rawArg s | null s    = NoArg
         | otherwise = RawArg s

rawDecls :: [TexDecl] -> LatexItem
rawDecls = mapNonEmpty $ fmap TexDecls . sequenceA

texDecl :: String -> TexDecl
texDecl s = pure $ TexDcl s []

texDecl' :: String -> [Arg LatexItem] -> TexDecl
texDecl' s opt = TexDcl s <$> mapM sequenceA opt

texDeclOpt :: String -> LatexItem -> TexDecl
texDeclOpt s opt = TexDcl s <$> ((:[]) . optional <$> opt)

parNote :: Note -> ParItem -> ParItem
parNote = fmap . ParNote

parCmdArgs :: String -> [Arg LatexItem] -> ParItem
parCmdArgs x ys = ParCmdArgs x <$> mapM sequenceA ys

parCmdArg :: String -> LatexItem -> ParItem
parCmdArg x y = parCmdArgs x [mandatory y]

latexNote :: Note -> LatexItem -> LatexItem
latexNote = fmap . LatexNote

latexCmdArgs :: String -> [Arg LatexItem] -> LatexItem
latexCmdArgs x ys = LatexCmdArgs x <$> mapM sequenceA ys

latexCmdArg :: String -> LatexItem -> LatexItem
latexCmdArg x y = latexCmdArgs x [mandatory y]

preambleNote :: Note -> PreambleItem -> PreambleItem
preambleNote = fmap . PreambleNote

preambleCmdArgs :: String -> [Arg LatexItem] -> PreambleItem
preambleCmdArgs x ys = PreambleCmdArgs x <$> mapM sequenceA ys

preambleCmdArg :: String -> LatexItem -> PreambleItem
preambleCmdArg x y = preambleCmdArgs x [mandatory y]

rawPreamble :: String -> PreambleItem
rawPreamble = mapNonEmpty $ pure . RawPreamble

size :: LatexSize -> LatexItem
size = pure . LatexSize

pkgName :: String -> PackageName
pkgName = PkgName

packageDependency :: PackageName -> Arg a
packageDependency = PackageDependency

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

listLikeEnv :: String -> [Arg LatexItem] -> [ListItem] -> ParItem
listLikeEnv name opts items =
  parEnvironmentPar name opts (mconcat <$> mapM (fmap mkItem) items)
  where mkItem (ListItm opts' contents) = ParCmdArgs "item" opts' <> contents

rawTex :: String -> LatexItem
rawTex = mapNonEmpty $ pure . RawTex

texCmdNoArg :: String -> LatexItem
texCmdNoArg = pure . TexCmdNoArg

latexKey :: Key -> LatexItem
latexKey = pure . LatexKeys . (:[])

latexKeys :: [Key] -> LatexItem
latexKeys = pure . LatexKeys

normSpaces :: String -> String
normSpaces = unlines . map (L.unwords . words) . lines

num :: Real a => a -> LatexItem
num = size . SizeRat . toRational

rat :: Rational -> LatexItem
rat = size . SizeRat

space :: LatexItem
space = rawTex "{ }"

-- TODO: make a safe version using a monad
-- fragile
unsafeNewsavebox :: Int -> (SaveBin, LatexItem)
unsafeNewsavebox n =
  let bin = UnsafeMakeSaveBin n
  in (bin, latexCmdArg "newsavebox" $ latexSaveBin bin)

-- sectioning

-- Sectioning commands arguments are 'moving'.
sectioning :: String -> ((LatexItem -> ParItem), (Star -> Maybe LatexItem -> LatexItem -> ParItem))
sectioning name = (sect, sect')
  where sect = sect' NoStar Nothing
        sect' star opt arg = parCmdArgs (name ++ addstar star)
                                        (maybeToList (fmap optional opt) ++ [mandatory arg])
        addstar Star   = "*"
        addstar NoStar = ""

unwords :: [LatexItem] -> LatexItem
unwords = mconcat . intersperse space

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
        err msg x op y = throwError $ L.unwords ["tabular:", msg, "(" ++ show x, op, show y ++ ")"] 

