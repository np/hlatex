{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, FlexibleContexts,
             UndecidableInstances, TemplateHaskell, MultiParamTypeClasses,
             DeriveDataTypeable #-}
module Language.LaTeX.Types where

import Prelude hiding (and, foldr, foldl, foldr1, foldl1, elem, concatMap, concat)
import Data.Monoid (Monoid(..))
import Data.List (intersperse)
import Data.Char
import Data.Ratio (Ratio, (%), numerator, denominator)
import Data.Traversable
import Data.Foldable
import Data.String (IsString(..))
import Data.Generics.PlateTypeable
import Control.Applicative
import Control.Monad.Writer (Writer)
import Control.Monad.State
import Control.Monad.Trans ()
import Control.Monad.Error
import Data.DeriveTH
import Data.Derive.Functor
import Data.Derive.Foldable
import Data.Derive.Traversable
import Data.Derive.PlateTypeable

data Root = Root PreambleItm Document
  deriving (Show, Eq, Typeable)

data Document = Document ParItm
  deriving (Show, Eq, Typeable)

data DocumentClass = Article
                   | Book
                   | Report
                   | Letter
                   | OtherDocumentClass String
  deriving (Show, Eq, Typeable)

data PreambleItm = PreambleCmd String
              | PreambleCmdArgs String [Arg LatexItm]
              | PreambleConcat [PreambleItm]
              | Usepackage PackageName [Arg LatexItm]
              | RawPreamble String
  deriving (Show, Eq, Typeable)

instance Monoid PreambleItm where
  mempty  = PreambleConcat []
  PreambleConcat xs `mappend` PreambleConcat ys = PreambleConcat (xs ++ ys)
  PreambleConcat xs `mappend` y                 = PreambleConcat (xs ++ [y])
  x                 `mappend` PreambleConcat ys = PreambleConcat (x : ys)
  x                 `mappend` y                 = PreambleConcat [x, y]

data TexDcl = TexDcl { texDeclName :: String
                     , texDeclPkg  :: Maybe PackageName
                     , texDeclArgs :: [Arg LatexItm]
                     }
  deriving (Show, Eq, Typeable)

data LatexItm
           = LatexCmdArgs String [Arg LatexItm]
           | TexDecls [TexDcl]
           | TexCmdNoArg String
           | TexCmdArg String LatexItm
           | Environment String [Arg LatexItm] LatexItm
           | MathInline MathItm
           | LatexCoord Coord
           | LatexSize LatexSize
           | LatexKeys [Key]
           | LatexSaveBin SaveBin
           | LatexParMode ParItm
           | LatexNeedPackage PackageName LatexItm
           | RawTex String
           | TexGroup LatexItm
           | LatexConcat [LatexItm]
  deriving (Show, Eq, Typeable)

instance Monoid LatexItm where
  mempty  = LatexConcat []
  LatexConcat xs `mappend` LatexConcat ys = LatexConcat (xs ++ ys)
  LatexConcat xs `mappend` y              = LatexConcat (xs ++ [y])
  x              `mappend` LatexConcat ys = LatexConcat (x : ys)
  x              `mappend` y              = LatexConcat [x, y]

instance IsString LatexItm where
  fromString = RawTex . concatMap hchar . concat . intersperse "\n" . filter (not . null) . lines

data Arg a = Optional a
           | Optionals [a]
           | Mandatory a
           | Coordinates a a
  deriving (Show, Eq, Typeable)

{-
instance Functor Arg where
  f `fmap` (Optional x) = Optional $ f x
  f `fmap` (Optionals xs) = Optionals $ fmap f xs
  f `fmap` (Mandatory x) = Mandatory $ f x
  f `fmap` (Coordinates x y) = Coordinates (f x) (f y)

instance Foldable Arg where
  foldr f z (Optional x) = f x z
  foldr f z (Optionals xs) = foldr f z xs
  foldr f z (Mandatory x) = f x z
  foldr f z (Coordinates x y) = f x (f y z)

instance Traversable Arg where
  sequenceA (Optional x) = Optional <$> x
  sequenceA (Optionals xs) = Optionals <$> sequenceA xs
  sequenceA (Mandatory x) = Mandatory <$> x
  sequenceA (Coordinates x y) = Coordinates <$> x <*> y
-}

data Coord = Coord LatexSize LatexSize
  deriving (Show, Eq, Typeable)

data ParItm  = Para LatexItm -- Here LatexItm does not mean LR mode
             | ParCmdArgs String [Arg LatexItm]
             | ParEnvironmentLR String LatexItm
             | ParEnvironmentPar String [Arg LatexItm] ParItm
             | DisplayMath MathItm
             | Equation [MathItm]
             | Tabular [RowSpec LatexItm] [Row LatexItm]
             | FigureLike String [LocSpec] ParItm
             | ParNeedPackage PackageName ParItm
             | RawParMode String
             | ParGroup ParItm -- check validity of this
             | ParConcat [ParItm]
  deriving (Show, Eq, Typeable)

instance Monoid ParItm where
  mempty  = ParConcat []
  ParConcat xs `mappend` ParConcat ys = ParConcat (xs ++ ys)
  ParConcat xs `mappend` y            = ParConcat (xs ++ [y])
  x            `mappend` ParConcat ys = ParConcat (x : ys)
  x            `mappend` y            = ParConcat [x, y]

newtype MathDcl = MathDcl String
  deriving (Show, Eq, Typeable)

data MathItm   = MathDecls [MathDcl]
               | MathCmdArgs String [Arg MathItm]
               | MathToLR String LatexItm
               | MathArray [RowSpec MathItm] [Row MathItm]
               | MathNeedPackage PackageName MathItm
               | RawMath String
               | MathRat Rational
               | MathGroup MathItm
               | MathConcat [MathItm]
               | MathBinOp String MathItm MathItm
               | MathUnOp String MathItm
  deriving (Show, Eq, Typeable)

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
  abs x = MathCmdArgs "abs" [Mandatory x] -- TODO check
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

data LatexSize = Sp Rational -- ^ Scalled point (1pt = 65536sp)
               | Pt Rational -- ^ Point unit size (1pt = 0.351mm)
               | Bp Rational -- ^ Big point (1in = 72bp), also PostScript point
               | Dd Rational -- ^ Didôt point (1dd = 0.376mm)
               | Em Rational -- ^ One em is about the width of the letter M in the current font
               | Ex Rational -- ^ One ex is about the hoigh of the letter x in the current font
               | Cm Rational -- ^ Centimeter unit size
               | Mm Rational -- ^ Milimeter unit size (1mm = 2.845pt)
               | In Rational -- ^ Inch unit size (1in = 72.27pt)
               | Pc Rational -- ^ Picas (1pc = 12pt)
               | Cc Rational -- ^ Cicero (1dd = 12dd = 4.531mm)
               | Mu Rational -- ^ Math unit (18mu = 1em)
               | SizeCmd String
               | SizeCmdRatArg String Rational
               | SizeBinOp String LatexSize LatexSize
               | SizeUnOp String LatexSize
               | SizeRat Rational
  deriving (Show, Eq, Typeable)

instance Num LatexSize where
  (+) = SizeBinOp "+"
  (*) = SizeBinOp "*"
  (-) = SizeBinOp "-"
  negate = SizeUnOp "-"
  abs = error "LatexSize.abs is undefined"
  signum = error "LatexSize.signum is undefined"
  fromInteger = SizeRat . (%1)

instance Fractional LatexSize where
  (/) = SizeBinOp "/"
  fromRational = SizeRat

-- p{wd}, and *{num}{cols} are explicitly
-- not supported, it seems much more natural and
-- simple to obtain the same goals using standard
-- programming uppon the rows and cells.
data RowSpec a = Rc --- ^ Centered
               | Rl --- ^ Left
               | Rr --- ^ Right
               | Rvline --- ^ A vertical line
               | Rtext a --- ^ A fixed text column (@-expression in LaTeX parlance)
  deriving (Show, Eq, Typeable)

{-
instance Functor RowSpec where
  _ `fmap` Rc         = Rc
  _ `fmap` Rl         = Rl
  _ `fmap` Rr         = Rr
  _ `fmap` Rvline     = Rvline
  f `fmap` (Rtext x)  = Rtext $ f x

instance Foldable RowSpec where
  foldr _ z Rc        = z
  foldr _ z Rl        = z
  foldr _ z Rr        = z
  foldr _ z Rvline    = z
  foldr f z (Rtext x) = f x z

instance Traversable RowSpec where
  sequenceA Rc        = pure Rc
  sequenceA Rl        = pure Rl
  sequenceA Rr        = pure Rr
  sequenceA Rvline    = pure Rvline
  sequenceA (Rtext x) = Rtext <$> x
-}

data LocSpec = Lh --- ^ Here
             | Lt --- ^ Top
             | Lb --- ^ Bottom
             | Lp --- ^ Page of floats: on a sperate page containing no text,
                  ---   only figures and tables.
  deriving (Show, Eq, Typeable)

locSpecChar :: LocSpec -> Char
locSpecChar Lh = 'h'
locSpecChar Lt = 't'
locSpecChar Lb = 'b'
locSpecChar Lp = 'p'

data Pos = Centered   -- ^ Centered (default).
         | FlushLeft  -- ^ Flush left
         | FlushRight -- ^ Flush right
         | Stretch -- ^ Stretch (justify) across entire width; text must contain
                   -- stretchable space for this to work.

charPos :: Pos -> Char
charPos Centered   = 'c'
charPos FlushLeft  = 'l'
charPos FlushRight = 'r'
charPos Stretch    = 's'

data LatexPaper = A4paper

{- NOTE: their is no handling of the \multicolumn command at the moment -}
data Row cell = Cells [cell]
              | Hline
              | Cline Int Int
  deriving (Show, Eq, Typeable)

{-
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
-}

data ListItm = ListItm { itemLabel :: Maybe LatexItm, itemContents :: ParItm }

newtype PackageName = PkgName { getPkgName :: String }
  deriving (Ord, Eq, Show, Typeable)

newtype Key = Key { getKey :: String }
  deriving (Eq, Show, Typeable)

newtype SaveBin = UnsafeMakeSaveBin { unsafeGetSaveBin :: Int }
  deriving (Eq, Show, Typeable)

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

type TexDecl   = LatexM TexDcl
type LatexItem = LatexM LatexItm
type ParItem   = LatexM ParItm
type MathDecl  = LatexM MathDcl
type MathItem  = LatexM MathItm
type ListItem  = LatexM ListItm
type PreambleItem = LatexM PreambleItm

type TexDeclW      = Writer TexDecl ()
type LatexItemW    = Writer LatexItem ()
type ParItemW      = Writer ParItem ()
type MathDeclW     = Writer MathDecl ()
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
        | x `elem` "]["      = ['{', x, '}'] -- to avoid mess up optional args
        | otherwise          = [x]

mchar :: Char -> String
mchar '\\' = "\\textbackslash{}"
mchar '~'  = "\\text{\\~{}}"
mchar '^'  = "\\^{}"
mchar ':'  = ":"
mchar x | x `elem` "#_&{}$%" = ['\\',x]
        | x `elem` "]["      = ['{', x, '}'] -- to avoid mess up optional args
        | otherwise          = [x]

instance (Integral a, Typeable a, Typeable b, PlateAll a b) => PlateAll (Ratio a) b where
  plateAll r = plate (%) |+ numerator r |+ denominator r


$(derive makeFunctor     ''Arg)
$(derive makeFoldable    ''Arg)
$(derive makeTraversable ''Arg)
$(derive makeFunctor     ''RowSpec)
$(derive makeFoldable    ''RowSpec)
$(derive makeTraversable ''RowSpec)
$(derive makeFunctor     ''Row)
$(derive makeFoldable    ''Row)
$(derive makeTraversable ''Row)

$(derive makePlateTypeable ''Arg)
$(derive makePlateTypeable ''TexDcl)
$(derive makePlateTypeable ''MathDcl)
$(derive makePlateTypeable ''LatexItm)
$(derive makePlateTypeable ''MathItm)
$(derive makePlateTypeable ''ParItm)
$(derive makePlateTypeable ''PreambleItm)
$(derive makePlateTypeable ''Key)
$(derive makePlateTypeable ''Row)
$(derive makePlateTypeable ''RowSpec)
$(derive makePlateTypeable ''LocSpec)
$(derive makePlateTypeable ''Coord)
$(derive makePlateTypeable ''LatexSize)
$(derive makePlateTypeable ''SaveBin)
$(derive makePlateTypeable ''PackageName)
$(derive makePlateTypeable ''Root)
$(derive makePlateTypeable ''Document)
$(derive makePlateTypeable ''DocumentClass)
