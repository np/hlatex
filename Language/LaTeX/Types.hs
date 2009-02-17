{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Language.LaTeX.Types where

import Prelude hiding (and, foldr, foldl, foldr1, foldl1, elem, concatMap, concat)
import Data.Monoid (Monoid(..))
import Data.List (intersperse)
import Data.Char
import Data.Ratio ((%))
import Data.Traversable
import Data.Foldable
import Data.String (IsString(..))
import Control.Applicative
import Control.Monad.Writer (Writer)
import Control.Monad.State
import Control.Monad.Trans ()
import Control.Monad.Error

data Root = Root PreambleItm Document

data Document = Document ParItm

data DocumentClass = Article
                   | Book
                   | Report
                   | Letter
                   | OtherDocumentClass String

data PreambleItm = PreambleCmd String
              | PreambleCmdArg String LatexItm
              | PreambleCmdArgWithOpts String [String] LatexItm
              | PreambleConcat [PreambleItm]

instance Monoid PreambleItm where
  mempty  = PreambleConcat []
  PreambleConcat xs `mappend` PreambleConcat ys = PreambleConcat (xs ++ ys)
  PreambleConcat xs `mappend` y                 = PreambleConcat (xs ++ [y])
  x                 `mappend` PreambleConcat ys = PreambleConcat (x : ys)
  x                 `mappend` y                 = PreambleConcat [x, y]

data LatexItm
           = LatexCmdArgs String [Arg LatexItm]
           | TexDecl String
           | TexDeclOpt String LatexItm
           | TexCmdNoArg String
           | TexCmdArg String LatexItm
           | Environment String [Arg LatexItm] LatexItm
           | MathInline MathItm
           | LatexSize LatexSize
           | LatexKeys [Key]
           | LatexSaveBin SaveBin
           | LatexParMode ParItm
           | RawTex String
           | TexGroup LatexItm
           | LatexConcat [LatexItm]
  deriving (Show, Eq)

instance Monoid LatexItm where
  mempty  = LatexConcat []
  LatexConcat xs `mappend` LatexConcat ys = LatexConcat (xs ++ ys)
  LatexConcat xs `mappend` y              = LatexConcat (xs ++ [y])
  x              `mappend` LatexConcat ys = LatexConcat (x : ys)
  x              `mappend` y              = LatexConcat [x, y]

instance IsString LatexItm where
  fromString = RawTex . concatMap hchar . concat . intersperse "\n" . filter (not . null) . lines

data Arg a = Arg ArgKind a
  deriving (Show, Eq)

instance Functor Arg where
  f `fmap` Arg k x = Arg k (f x)

instance Foldable Arg where
  foldr f z (Arg _ x) = f x z

instance Traversable Arg where
  sequenceA (Arg k x) = Arg k <$> x

data ArgKind = Optional | Mandatory | Coordinate
  deriving (Show, Eq)

data ParItm  = Para LatexItm -- Here LatexItm does not mean LR mode
             | ParDecl String
             | ParDeclOpt String LatexItm
             | ParCmdArgs String [Arg LatexItm]
             | ParEnvironmentLR String LatexItm
             | ParEnvironmentPar String [Arg LatexItm] ParItm
             | DisplayMath MathItm
             | Equation [MathItm]
             | Tabular [RowSpec] [Row LatexItm]
             | FigureLike String [LocSpec] ParItm
             | RawParMode String
             | ParGroup ParItm -- check validity of this
             | ParConcat [ParItm]
  deriving (Show, Eq)

instance Monoid ParItm where
  mempty  = ParConcat []
  ParConcat xs `mappend` ParConcat ys = ParConcat (xs ++ ys)
  ParConcat xs `mappend` y            = ParConcat (xs ++ [y])
  x            `mappend` ParConcat ys = ParConcat (x : ys)
  x            `mappend` y            = ParConcat [x, y]

data MathItm   = MathDecl String
               | MathCmdArgs String [Arg MathItm]
               | MathToLR String LatexItm
               | MathArray [RowSpec] [Row MathItm]
               | MathNeedPackage String MathItm
               | RawMath String
               | MathRat Rational
               | MathGroup MathItm
               | MathConcat [MathItm]
               | MathBinOp String MathItm MathItm
               | MathUnOp String MathItm
  deriving (Show, Eq)

instance Monoid MathItm where
  mempty  = MathConcat []
  MathConcat xs `mappend` MathConcat ys = MathConcat (xs ++ ys)
  MathConcat xs `mappend` y              = MathConcat (xs ++ [y])
  x              `mappend` MathConcat ys = MathConcat (x : ys)
  x              `mappend` y              = MathConcat [x, y]

instance Num MathItm where
  (+) = MathBinOp "+"
  (*) = MathBinOp "*"
  (-) = MathBinOp "-"
  negate = MathUnOp "-"
  abs x = MathCmdArgs "abs" [Arg Mandatory x] -- TODO check
  signum = error "MathItm.signum is undefined"
  fromInteger = MathRat . (%1)

instance Fractional MathItm where
  (/) = MathBinOp "/"
  fromRational = MathRat

{-
instance Real MathItm where
  toRational = error "MathItm.toRational is undefined"

instance Integral MathItm where
  mod = MathBinOp "bmod"
  -- TODO quot, rem
-}

data LatexSize = Pt Rational -- ^ Point unit size
               | Em Rational -- ^ One em is about the width of the letter M in the current font
               | Ex Rational -- ^ One ex is about the hoigh of the letter x in the current font
               | Cm Rational -- ^ Centimeter unit size
               | Mm Rational -- ^ Milimeter unit size
               | In Rational -- ^ Inch unit size (1in = 72.27pt)
               | Pc Rational -- ^ Picas (1pc = 12pt)
               | SizeCmd String
               | SizeCmdRatArg String Rational
               | SizeBinOp String LatexSize LatexSize
               | SizeUnOp String LatexSize
               | SizeInt Integer
  deriving (Show, Eq)

instance Num LatexSize where
  (+) = SizeBinOp "+"
  (*) = SizeBinOp "*"
  (-) = SizeBinOp "-"
  negate = SizeUnOp "-"
  abs = error "LatexSize.abs is undefined"
  signum = error "LatexSize.signum is undefined"
  fromInteger = SizeInt

-- @{text}, p{wd}, and *{num}{cols} are explicitly
-- not supported, it seems much more natural and
-- simple to obtain the same goals using standard
-- programming uppon the rows and cells.
data RowSpec = Rc --- ^ Centered
             | Rl --- ^ Left
             | Rr --- ^ Right
             | Rvline --- ^ A vertical line
  deriving (Show, Eq)

rowSpecChar :: RowSpec -> Char
rowSpecChar Rc     = 'c'
rowSpecChar Rl     = 'l'
rowSpecChar Rr     = 'r'
rowSpecChar Rvline = '|'

data LocSpec = Lh --- ^ Here
             | Lt --- ^ Top
             | Lb --- ^ Bottom
             | Lp --- ^ Page of floats: on a sperate page containing no text,
                  ---   only figures and tables.
  deriving (Show, Eq)

locSpecChar :: LocSpec -> Char
locSpecChar Lh = 'h'
locSpecChar Lt = 't'
locSpecChar Lb = 'b'
locSpecChar Lp = 'p'

data LatexPaper = A4paper

{- NOTE: their is no handling of the \multicolumn command at the moment -}
data Row cell = Cells [cell]
              | Hline
              | Cline Int Int
  deriving (Show, Eq)

instance Functor Row where
  f `fmap` Cells cs  = Cells (fmap f cs)
  _ `fmap` Hline     = Hline
  _ `fmap` Cline i j = Cline i j

instance Foldable Row where
  foldr f z (Cells cs)  = foldr f z cs
  foldr _ z Hline       = z
  foldr _ z (Cline _ _) = z

instance Traversable Row where
  sequenceA (Cells cs)  = Cells <$> sequenceA cs
  sequenceA Hline       = pure Hline
  sequenceA (Cline i j) = pure $ Cline i j

data ListItm = ListItm { itemLabel :: Maybe LatexItm, itemContents :: ParItm }

newtype Key = Key { getKey :: String }
  deriving (Eq, Show)

newtype SaveBin = UnsafeMakeSaveBin { unsafeGetSaveBin :: Int }
  deriving (Eq, Show)

data Star = Star | NoStar

data LatexState = LS { freshSaveBin :: SaveBin }

instance Monad m => Applicative (StateT LatexState m) where
  pure = return
  (<*>) = ap

{-
instance Num a => Num (StateT LatexState (Either String) a) where
  fromInteger = pure . fromInteger

instance Eq a => Eq (StateT LatexState (Either String) a) where
  (==) = liftM2 (==)

instance Show a => Show (StateT LatexState (Either String) a) where
  showsPrec = liftM . showsPrec
  -- show = liftM . show

newtype LatexM a = LatexM { runLatexM :: StateT LatexState (Either String) a } 
  deriving (Functor, Applicative, Monad, MonadPlus,
            MonadState LatexState, MonadError String, Show, Eq, Num)
-}

instance Applicative (Either a) where
  pure = Right
  _ <*> Left x = Left x
  Left x <*> _ = Left x
  Right f <*> Right x = Right (f x)

instance (Error a, Eq a, Show a, Num b) => Num (Either a b) where
  fromInteger = pure . fromInteger
  (+)         = liftM2 (+)
  (-)         = liftM2 (-)
  (*)         = liftM2 (*)
  abs         = liftM abs
  signum      = liftM signum

instance (Error a, Eq a, Show a, Fractional b) => Fractional (Either a b) where
  (/) = liftM2 (/)
  fromRational = pure . fromRational

newtype LatexM a = LatexM { runLatexM :: Either String a } 
  deriving (Functor, Applicative, Monad, MonadPlus,
            MonadError String, Show, Eq, Num, Fractional)

instance Monoid a => Monoid (LatexM a) where
  mempty = pure mempty
  mappend = liftM2 mappend
  mconcat = liftM mconcat . sequenceA

instance IsString a => IsString (LatexM a) where fromString = pure . fromString

type LatexItem = LatexM LatexItm
type ParItem   = LatexM ParItm
type MathItem  = LatexM MathItm
type ListItem  = LatexM ListItm
type PreambleItem = LatexM PreambleItm

type LatexItemW    = Writer LatexItem ()
type ParItemW      = Writer ParItem ()
type MathItemW     = Writer MathItem ()
type PreambleItemW = Writer PreambleItem ()

showPaper :: LatexPaper -> String
showPaper A4paper = "a4paper"

showDocumentClass :: DocumentClass -> String
showDocumentClass Article = "article"
showDocumentClass Book    = "book"
showDocumentClass Report  = "report"
showDocumentClass Letter  = "letter"
showDocumentClass (OtherDocumentClass x) = x

-- TODO: Maybe one should handle quotes in a less LaTeX
-- way: provide a replacement for ``...'' and escape `'"
hchar :: Char -> String
hchar '\\' = "\\textbackslash{}"
hchar '~'  = "\\~{}"
hchar '<'  = "\\textless{}"
hchar '>'  = "\\textgreater{}"
hchar '^'  = "\\^{}"
hchar '|'  = "\\textbar{}"
hchar ':'  = "$:$" -- or maybe "{:}"
hchar x | x `elem` "#_&{}$%" = ['\\',x]
        | x `elem` "-]["     = ['{', x, '}'] -- to avoid multiple dashes or mess up optional args
        | otherwise          = [x]

mchar :: Char -> String
mchar '\\' = "\\textbackslash{}"
mchar '~'  = "\\text{\\~{}}"
mchar '^'  = "\\^{}"
mchar ':'  = ":"
mchar x | x `elem` "#_&{}$%" = ['\\',x]
        | x `elem` "-]["     = ['{', x, '}'] -- to avoid multiple dashes or mess up optional args
        | otherwise          = [x]
