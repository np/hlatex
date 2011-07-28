{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, DeriveDataTypeable #-}

-- Those extensions are required by the Uniplate instances.
--{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Language.LaTeX.Types where

import Prelude hiding (and, foldr, foldl, foldr1, foldl1, elem, concatMap, concat)
import Data.Monoid (Monoid(..))
import Data.List (intersperse)
import Data.Ratio ((%))
import Data.Traversable
import Data.Foldable
import Data.String (IsString(..))
-- import Data.Generics.PlateTypeable
import Data.Data
import Data.DeriveTH
import Control.Applicative
import Control.Arrow (second)
import Control.Monad.Writer (Writer)
import Control.Monad.Trans ()
import Control.Monad.Error

data Document = Document { documentClass     :: DocumentClss
                         , documentPreamble  :: PreambleItm
                         , documentBody      :: ParItm }
  deriving (Show, Eq, Typeable, Data)

type LineNumber = Int
type CharNumber = Int

data Loc = Loc { locFile :: FilePath
               , locLine :: LineNumber
               , locChar :: CharNumber
               }
   deriving (Show, Eq, Typeable, Data)

data Note = TextNote String
          | IntNote Int
          | LocNote Loc
   deriving (Show, Eq, Typeable, Data)

data DocumentClassKind  = Article
                        | Book
                        | Report
                        | Letter
                        | OtherDocumentClassKind String
  deriving (Show, Eq, Typeable, Data)

data DocumentClss
  = DocClass  {  docClassKind     :: DocumentClassKind
              ,  docClassOptions  :: [AnyItm]
              }
  deriving (Show, Eq, Typeable, Data)

data AnyItm = PreambleItm PreambleItm
            | LatexItm    LatexItm
            | MathItm     MathItm
            | ParItm      ParItm
            | LocSpecs    [LocSpec]
            | Key         Key
            | PackageName PackageName
            | Coord       Coord
            | Length      LatexLength
            | SaveBin     SaveBin
  deriving (Show, Eq, Typeable, Data)

data PreambleItm = PreambleCmdArgs String [Arg AnyItm]
                 | PreambleEnv String [Arg AnyItm] AnyItm
                 | PreambleCast AnyItm
                 | PreambleConcat [PreambleItm]
                 | RawPreamble String
                 | PreambleNote Key Note PreambleItm
  deriving (Show, Eq, Typeable, Data)

instance Monoid PreambleItm where
  mempty  = PreambleConcat []
  PreambleConcat xs `mappend` PreambleConcat ys = PreambleConcat (xs ++ ys)
  PreambleConcat xs `mappend` y                 = PreambleConcat (xs ++ [y])
  x                 `mappend` PreambleConcat ys = PreambleConcat (x : ys)
  x                 `mappend` y                 = PreambleConcat [x, y]

data TexDcl = TexDcl { texDeclName :: String
                     , texDeclArgs :: [Arg AnyItm]
                     }
  deriving (Show, Eq, Typeable, Data)

-- This is the subset of tex strings that fits in the LR mode.
-- The distinction between the paragraph and the LR mode is always
-- made explicit using the Para constructor.
--
-- Modes: http://www.personal.ceu.hu/tex/modes.htm
data LatexItm
           = LatexCmdArgs String [Arg LatexItm]
           | LatexCmdAnyArgs String [Arg AnyItm]
           | TexDecls [TexDcl]
           | TexCmdNoArg String
           | TexCmdArg String LatexItm
           | Environment String [Arg AnyItm] AnyItm
           | RawTex String
           | LatexCast AnyItm -- a cast from math induce $...$
           | TexGroup LatexItm
           | LatexConcat [LatexItm]
           | LatexNote Key Note LatexItm
  deriving (Show, Eq, Typeable, Data)

{-
-- (+++) :: Monoid m => [m] -> [m] -> [m]
(+++) :: [LatexItm] -> [LatexItm] -> [LatexItm]
[x]    +++ (y:ys) = unLatexConcat $ mconcat (x : y : ys)
[]     +++ xs     = xs
(x:xs) +++ ys     = x : (xs +++ ys)

unLatexConcat :: LatexItm -> [LatexItm]
unLatexConcat (LatexConcat xs) = xs
unLatexConcat x                = [x]

mappendAny :: AnyItm -> AnyItm -> [AnyItm]
mappendAny (PreambleItm x) (PreambleItm y) = [PreambleItm (x `mappend` y)]
mappendAny (LatexItm x)    (LatexItm y) = [LatexItm (x `mappend` y)]
mappendAny (MathItm x)     (MathItm y) = [MathItm (x `mappend` y)]
mappendAny (ParItm x)      (ParItm y) = [ParItm (x `mappend` y)]
mappendAny (LocSpecs x)    (LocSpecs y) = [LocSpecs (x `mappend` y)]
-- this lengthy matching is to get a warning when we extend the AnyItm type
mappendAny x@PreambleItm{} y = [x, y]
mappendAny x@LatexItm{}    y = [x, y]
mappendAny x@MathItm{}     y = [x, y]
mappendAny x@ParItm{}      y = [x, y]
mappendAny x@LocSpecs{}    y = [x, y]
mappendAny x@Key{}         y = [x, y]
mappendAny x@PackageName{} y = [x, y]
mappendAny x@Coord{}       y = [x, y]
mappendAny x@Length{}      y = [x, y]
mappendAny x@SaveBin{}     y = [x, y]
-}

instance Monoid LatexItm where
  mempty  = LatexConcat []
{-
  LatexConcat xs `mappend` LatexConcat ys = LatexConcat (xs  +++ ys)
  LatexConcat xs `mappend` y              = LatexConcat (xs  +++ [y])
  x              `mappend` LatexConcat ys = LatexConcat ([x] +++ ys)
  LatexCast x    `mappend` LatexCast y    = LatexConcat (map LatexCast (mappendAny x y))
-}
  LatexConcat xs `mappend` LatexConcat ys = LatexConcat (xs  ++ ys)
  LatexConcat xs `mappend` y              = LatexConcat (xs  ++ [y])
  x              `mappend` LatexConcat ys = LatexConcat ([x] ++ ys)
  x              `mappend` y              = LatexConcat [x, y]

instance IsString LatexItm where
  fromString s
    | null s    = mempty
    | otherwise = f s
    where f = RawTex . concatMap rawhchar . concat . intersperse "\n" . filter (not . null) . lines

data Named a = Named String a
  deriving (Show, Eq, Typeable, Data)

data PackageAction = PackageDependency PackageName
                   | ProvidePackage    PackageName
  deriving (Show, Eq, Typeable, Data)

data Arg a = NoArg
           | StarArg             -- `*'
           | Mandatory [a]       -- surrounded by `{' `}', separated by `,'
           | Optional [a]        -- surrounded by `[' `]', separated by `,' or empty if null
           | NamedArgs [Named a] -- surrounded by `{' `}', separated by `=` and `,'
           | NamedOpts [Named a] -- surrounded by `[' `]', separated by `=' and `,' or empty if null
           | Coordinates a a     -- surrounded by `(' `)', separated by ` '
           | RawArg String
           | PackageAction PackageAction
  deriving (Show, Eq, Typeable, Data)

data Star = Star | NoStar
  deriving (Show, Eq, Typeable, Data)

instance Monoid Star where
  mempty = NoStar
  NoStar `mappend` x      = x
  x      `mappend` _      = x

data Coord = MkCoord LatexLength LatexLength
  deriving (Show, Eq, Typeable, Data)

newtype Percentage = Percentage { percentage :: Int } deriving (Eq,Show,Ord,Num)

data ParItm  = ParCmdArgs String [Arg AnyItm]
             | ParEnv String [Arg AnyItm] AnyItm
             | Tabular [RowSpec LatexItm] [Row LatexItm]
             | RawParMode String
             | ParCast AnyItm -- a cast from Math induce \[...\]
             | ParGroup ParItm -- check validity of this
             | ParConcat [ParItm]
             | ParNote Key Note ParItm
  deriving (Show, Eq, Typeable, Data)

instance Monoid ParItm where
  mempty  = ParConcat []
  ParConcat xs `mappend` ParConcat ys = ParConcat (xs ++ ys)
  ParConcat xs `mappend` y            = ParConcat (xs ++ [y])
  x            `mappend` ParConcat ys = ParConcat (x : ys)
  x            `mappend` y            = ParConcat [x, y]

uncatParItm :: ParItm -> [ParItm]
uncatParItm (ParConcat pars) = pars
uncatParItm par              = [par]

newtype MathDcl = MathDcl String
  deriving (Show, Eq, Typeable, Data)

data MathItm   = MathDecls [MathDcl]
               | MathCmdArgs String [Arg AnyItm]
               | MathArray [RowSpec MathItm] [Row MathItm]
               | RawMath String
               | MathCast AnyItm
               | MathRat Rational
               | MathGroup MathItm
               | MathConcat [MathItm]
               | MathBinOp String MathItm MathItm
               | MathUnOp String MathItm
               | MathNote Key Note MathItm
  deriving (Show, Eq, Typeable, Data)

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
  abs x = MathCmdArgs "abs" [Mandatory [MathItm x]] -- TODO check
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

data TexUnit
  = Sp -- ^ Scalled point (1pt = 65536sp)
  | Pt -- ^ Point unit size (1pt = 0.351mm)
  | Bp -- ^ Big point (1in = 72bp), also PostScript point
  | Dd -- ^ Didôt point (1dd = 0.376mm)
  | Em -- ^ One em is about the width of the letter M in the current font
  | Ex -- ^ One ex is about the hoigh of the letter x in the current font
  | Cm -- ^ Centimeter unit size
  | Mm -- ^ Milimeter unit size (1mm = 2.845pt)
  | In -- ^ Inch unit size (1in = 72.27pt)
  | Pc -- ^ Picas (1pc = 12pt)
  | Cc -- ^ Cicero (1dd = 12dd = 4.531mm)
  | Mu -- ^ Math unit (18mu = 1em)
  deriving (Eq, Ord, Enum, Show, Typeable, Data)

data LatexLength = LengthScaledBy Rational LatexLength
                 | LengthCmdRatArg String Rational
                 | LengthCmd String
                 | LengthCst (Maybe TexUnit) Rational
  deriving (Show, Eq, Typeable, Data)

lengthCst :: LatexLength -> Maybe (Maybe TexUnit, Rational)
lengthCst (LengthScaledBy rat len) = second (rat *) <$> lengthCst len
lengthCst (LengthCmdRatArg _ _)    = Nothing
lengthCst (LengthCmd _)            = Nothing
lengthCst (LengthCst mtu rat)      = Just (mtu, rat)

safeLengthOp :: String -> (Rational -> Rational -> Rational) -> LatexLength -> LatexLength -> LatexLength
safeLengthOp _ op (LengthCst Nothing     rx) (LengthCst munit ry)
  = LengthCst munit (op rx ry)
safeLengthOp _ op (LengthCst (Just unit) rx) (LengthCst Nothing ry)
  = LengthCst (Just unit) (op rx ry)
safeLengthOp op _ x y
  = error $ "LatexLength." ++ op
         ++ ": undefined on non constants terms (" ++ show x ++ op ++ show y ++ ")"

scaleBy :: Rational -> LatexLength -> LatexLength
scaleBy rx (LengthScaledBy ry l)   = LengthScaledBy (rx * ry) l
scaleBy rx (LengthCst munit ry)    = LengthCst munit (rx * ry)
scaleBy rx (LengthCmd cmd)         = LengthScaledBy rx (LengthCmd cmd)
scaleBy rx (LengthCmdRatArg cmd r) = LengthScaledBy rx (LengthCmdRatArg cmd r)

instance Num LatexLength where
  LengthCst Nothing x * y = scaleBy x y
  x * LengthCst Nothing y = scaleBy y x
  x * y                   = safeLengthOp "*" (*) x y

  (+) = safeLengthOp "+" (+)
  (-) = safeLengthOp "-" (-)
  negate x = LengthCst Nothing (-1) * x
  abs = error "LatexLength.abs is undefined"
  signum = error "LatexLength.signum is undefined"
  fromInteger = LengthCst Nothing . (%1)

instance Monoid LatexLength where
  mempty = 0
  mappend = (+)

instance Fractional LatexLength where
  x / LengthCst Nothing ry = scaleBy (1/ry) x
  x / y                    = safeLengthOp "/" (/) x y
  fromRational = LengthCst Nothing

-- p{wd}, and *{num}{cols} are explicitly
-- not supported, it seems much more natural and
-- simple to obtain the same goals using standard
-- programming uppon the rows and cells.
data RowSpec a = Rc --- ^ Centered
               | Rl --- ^ Left
               | Rr --- ^ Right
               | Rvline --- ^ A vertical line
               | Rtext a --- ^ A fixed text column (@-expression in LaTeX parlance)
  deriving (Show, Eq, Typeable, Data)

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
  deriving (Show, Eq, Typeable, Data)

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

-- TODO: add more paper sizes
data LatexPaperSize = A4paper | OtherPaperSize String
  deriving (Show, Eq, Typeable, Data)

{- NOTE: their is no handling of the \multicolumn command at the moment -}
data Row cell = Cells [cell]
              | Hline
              | Cline Int Int
  deriving (Show, Eq, Typeable, Data)

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

data ListItm = ListItm { itemOptions :: [Arg LatexItm], itemContents :: ParItm }

newtype PackageName = PkgName { getPkgName :: String }
  deriving (Ord, Eq, Show, Typeable, Data)

newtype Key = MkKey { getKey :: String }
  deriving (Eq, Show, Typeable, Data)

newtype SaveBin = UnsafeMakeSaveBin { unsafeGetSaveBin :: Int }
  deriving (Eq, Show, Typeable, Data)

data LatexState = LS { freshSaveBin :: SaveBin }

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

type ErrorMessage = String

newtype LatexM a = LatexM { runLatexM :: Either ErrorMessage a }
  deriving (Functor, Applicative, Monad, MonadPlus,
            MonadError ErrorMessage, Show, Eq, Num, Fractional,
            Typeable, Data)

instance Monoid a => Monoid (LatexM a) where
  mempty = pure mempty
  mappend = liftM2 mappend
  mconcat = liftM mconcat . sequenceA

instance IsString a => IsString (LatexM a) where fromString = pure . fromString

type TexDecl   = LatexM TexDcl
type LatexItem = LatexM LatexItm
type ParItem   = LatexM ParItm
type MathDecl  = LatexM MathDcl
newtype AnyItem   = AnyItem { anyItmM :: LatexM AnyItm }
  deriving (Eq, Show, Typeable, Data)
newtype MathItem  = MathItem { mathItmM :: LatexM MathItm }
  deriving (Monoid, Eq, Show, Num, Fractional, Typeable, Data)
type ListItem  = LatexM ListItm
type PreambleItem = LatexM PreambleItm
type DocumentClass = LatexM DocumentClss

type TexDeclW      = Writer TexDecl ()
type LatexItemW    = Writer LatexItem ()
type ParItemW      = Writer ParItem ()
type MathDeclW     = Writer MathDecl ()
type MathItemW     = Writer MathItem ()
type PreambleItemW = Writer PreambleItem ()

-- TODO: Maybe one should handle quotes in a less LaTeX
-- way: provide a replacement for ``...'' and escape `'"
--
-- Don't confuse this function with ttchar
rawhchar :: Char -> String
rawhchar '\\'  = "\\textbackslash{}"
rawhchar '~'   = "\\~{}"
rawhchar '<'   = "\\textless{}"
rawhchar '>'   = "\\textgreater{}"
rawhchar '^'   = "\\^{}"
rawhchar '|'   = "\\textbar{}"
rawhchar ':'   = "$:$" -- or maybe "{:}"
rawhchar '_'   = "\\_"
rawhchar x  | x `elem` "#&{}$%"  = ['\\',x]
            | x `elem` "]["      = ['{', x, '}'] -- to avoid mess up optional args
            | otherwise          = [x]

-- | Type for encodings used in commands like.
-- @\usepackage[utf8]{inputenc}@, that we can
-- express as 'useInputenc' 'utf8'.
newtype Encoding = Encoding { fromEncoding :: String }
  deriving (Eq,Ord,Show)

-- instance (Integral a, Typeable a, Typeable b, PlateAll a b) => PlateAll (Ratio a) b where
--   plateAll r = plate (%) |+ numerator r |+ denominator r

$(derive makeFunctor     ''Named)
$(derive makeFoldable    ''Named)
$(derive makeTraversable ''Named)
$(derive makeFunctor     ''Arg)
$(derive makeFoldable    ''Arg)
$(derive makeTraversable ''Arg)
$(derive makeFunctor     ''RowSpec)
$(derive makeFoldable    ''RowSpec)
$(derive makeTraversable ''RowSpec)
$(derive makeFunctor     ''Row)
$(derive makeFoldable    ''Row)
$(derive makeTraversable ''Row)

{-
$(derive makeUniplateTypeable ''SaveBin)
$(derive makeUniplateTypeable ''PackageName)
$(derive makeUniplateTypeable ''Loc)
$(derive makeUniplateTypeable ''Note)
$(derive makeUniplateTypeable ''Arg)
$(derive makeUniplateTypeable ''Key)
$(derive makeUniplateTypeable ''Row)
$(derive makeUniplateTypeable ''RowSpec)
$(derive makeUniplateTypeable ''LocSpec)
$(derive makeUniplateTypeable ''LatexLength)
$(derive makeUniplateTypeable ''Coord)
$(derive makeUniplateTypeable ''DocumentClassKind)

$(derive makeUniplateTypeable ''TexDcl)
$(derive makeUniplateTypeable ''MathItm)
$(derive makeUniplateTypeable ''MathDcl)
$(derive makeUniplateTypeable ''ParItm)
$(derive makeUniplateTypeable ''TexUnit)
$(derive makeUniplateTypeable ''LatexItm)
$(derive makeUniplateTypeable ''DocumentClass)
$(derive makeUniplateTypeable ''PreambleItm)
$(derive makeUniplateTypeable ''Document)
-}
