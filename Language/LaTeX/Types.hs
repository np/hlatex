module Language.LaTeX.Types where

import Prelude hiding (and)
import Data.Monoid ()
import Data.List hiding (and)
import Data.Char
import Data.Ratio ((%))
import Control.Monad.Writer

data Root = Root Preamble Document

data Document = Document ParMode

data DocumentClass = Article
                   | Book
                   | Report
                   | Letter
                   | OtherDocumentClass String

data Preamble = PreambleCmd String
              | PreambleCmdArg String Latex
              | PreambleCmdArgWithOpts String [String] Latex
              | PreambleConcat [Preamble]

instance Monoid Preamble where
  mempty  = PreambleConcat []
  PreambleConcat xs `mappend` PreambleConcat ys = PreambleConcat (xs ++ ys)
  PreambleConcat xs `mappend` y                 = PreambleConcat (xs ++ [y])
  x                 `mappend` PreambleConcat ys = PreambleConcat (x : ys)
  x                 `mappend` y                 = PreambleConcat [x, y]

data Latex = LatexCmd String Latex
           | LatexCmdArgs String [Arg Latex]
           | TexDecl String
           | TexDeclOpt String Latex
           | TexCmdNoArg String
           | TexCmdArg String Latex
           | Environment String [Arg Latex] Latex
           | MathsInline MathsItem
           | LatexSize LatexSize
           | LatexKeys [Key]
           | LatexSaveBin SaveBin
           | LatexParMode ParMode
           | RawTex String
           | TexGroup Latex
           | LatexConcat [Latex]
  deriving (Show, Eq)

instance Monoid Latex where
  mempty  = LatexConcat []
  LatexConcat xs `mappend` LatexConcat ys = LatexConcat (xs ++ ys)
  LatexConcat xs `mappend` y              = LatexConcat (xs ++ [y])
  x              `mappend` LatexConcat ys = LatexConcat (x : ys)
  x              `mappend` y              = LatexConcat [x, y]

data Arg a = Arg ArgKind a
  deriving (Show, Eq)

instance Functor Arg where
  f `fmap` Arg k x = Arg k (f x)

data ArgKind = Optional | Mandatory | Coordinate
  deriving (Show, Eq)

data ParMode = Para Latex -- Here Latex does not mean LR mode
             | ParDecl String
             | ParDeclOpt String Latex
             | ParCmdArgs String [Arg Latex]
             | ParEnvironmentLR String Latex
             | ParEnvironmentPar String [Arg Latex] ParMode
             | DisplayMaths MathsItem
             | Equation [MathsItem]
             | Tabular [RowSpec] [Row Latex]
             | FigureLike String [LocSpec] ParMode
             | RawParMode String
             | ParGroup ParMode -- check validity of this
             | ParConcat [ParMode]
  deriving (Show, Eq)

instance Monoid ParMode where
  mempty  = ParConcat []
  ParConcat xs `mappend` ParConcat ys = ParConcat (xs ++ ys)
  ParConcat xs `mappend` y            = ParConcat (xs ++ [y])
  x            `mappend` ParConcat ys = ParConcat (x : ys)
  x            `mappend` y            = ParConcat [x, y]

data MathsItem = MathsCmd String
               | MathsDecl String
               | MathsCmdArgs String [Arg MathsItem]
               | MathsToLR String Latex
               | MathsArray [RowSpec] [Row MathsItem]
               | MathsNeedsPackage String MathsItem
               | RawMaths String
               | MathsRat Rational
               | MathsGroup MathsItem
               | MathsConcat [MathsItem]
               | MathsBinOp String MathsItem MathsItem
               | MathsUnOp String MathsItem
  deriving (Show, Eq)

instance Monoid MathsItem where
  mempty  = MathsConcat []
  MathsConcat xs `mappend` MathsConcat ys = MathsConcat (xs ++ ys)
  MathsConcat xs `mappend` y              = MathsConcat (xs ++ [y])
  x              `mappend` MathsConcat ys = MathsConcat (x : ys)
  x              `mappend` y              = MathsConcat [x, y]

instance Num MathsItem where
  (+) = MathsBinOp "+"
  (*) = MathsBinOp "*"
  (-) = MathsBinOp "-"
  negate = MathsUnOp "-"
  abs x = MathsCmdArgs "abs" [Arg Mandatory x] -- TODO check
  signum = error "MathsItem.signum is undefined"
  fromInteger = MathsRat . (%1)

instance Fractional MathsItem where
  (/) = MathsBinOp "/"
  fromRational = MathsRat

{-
instance Real MathsItem where
  toRational = error "MathsItem.toRational is undefined"

instance Integral MathsItem where
  mod = MathsBinOp "bmod"
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

data Item = Item { itemLabel :: Maybe Latex, itemContents :: ParMode }

newtype Key = Key { getKey :: String }
  deriving (Eq, Show)

newtype SaveBin = UnsafeMakeSaveBin { unsafeGetSaveBin :: Int }
  deriving (Eq, Show)

data Star = Star | NoStar

type LatexM = Writer Latex ()

showPaper :: LatexPaper -> String
showPaper A4paper = "a4paper"

showDocumentClass :: DocumentClass -> String
showDocumentClass Article = "article"
showDocumentClass Book    = "book"
showDocumentClass Report  = "report"
showDocumentClass Letter  = "letter"
showDocumentClass (OtherDocumentClass x) = x
