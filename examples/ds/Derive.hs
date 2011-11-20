module Derive where

import Text.PrettyPrint.Free
import Data.Ratio
import Data.Char (ord)
import Data.List (genericReplicate)
import Numeric

infixl 6 :+
-- infixl 6 +:
infixl 7 :*
infixl 7 .*

data Exp a
  = Val a
  | X a
  | (:+) (Exp a) (Exp a)
  | (:*) a (Exp a)
  | Recip (Exp a)
  deriving (Show,Eq)

{-
class EXP r where
  val    :: a -> r a
  _X     :: Int -> r a
  (+:)   :: r a -> r a -> r a
  (.*)   :: a -> r a -> r a
  recipT :: r a -> r a

instance EXP Exp where
  val = Val
  _X = X
  (+:) = (:+)
  (.*) = (:*)
  recipT = Recip
-}

(.*) :: a -> Exp a -> Exp a
(.*) = (:*)

instance (Integral a, Pretty a) => Pretty (Ratio a) where
  pretty r =
    let den = denominator r
        num = numerator   r in
    if den == 1 then pretty num
    else parens (pretty num <+> text "/" <+> pretty den)

class Pretty a => PrettySup a where
  prettySup :: a -> Doc e
  prettySup = pretty

instance PrettySup Int where
  prettySup n = text (showIntAtBase 10 ("⁰¹²³⁴⁵⁶⁷⁸⁹"!!) n "")

instance PrettySup Integer where
  prettySup = text . map f . show
    where f = ("⁰¹²³⁴⁵⁶⁷⁸⁹"!!) . subtract (ord '0') . ord

instance (Integral a, PrettySup a) => PrettySup (Ratio a) where
  prettySup r =
    let den = denominator r
        num = numerator   r in
    if den == 1 then prettySup num
    else text "^" <> parens (pretty num <+> text "/" <+> pretty den)

type Prec = Int

type PDoc e = Prec -> Doc e

appPrec, topPrec :: Prec
appPrec = 10
topPrec = 0

prettyP :: PDoc e -> Doc e
prettyP f = f topPrec

txt :: String -> PDoc e
txt = const . text

parenIf :: (Prec -> Bool) -> Doc e -> PDoc e
parenIf p doc prec = if p prec then parens doc else doc

infixrP, infixlP :: String -> Prec -> PDoc e -> PDoc e -> PDoc e
infixGenP :: (Int -> Int) -> (Int -> Int) -> String -> Prec -> PDoc e -> PDoc e -> PDoc e

infixGenP f g op prec t u = parenIf (>= prec) . group . align $ t (f prec) <+> text op `above` u (g prec)
infixlP = infixGenP pred id
infixrP = infixGenP id pred

appDoc :: PDoc e -> PDoc e -> PDoc e
appDoc t u = parenIf (>= appPrec) . group . hang 2 $ t (pred appPrec) `above` u appPrec

prettyExp :: (Fractional a, PrettySup a) => Exp a -> PDoc e
prettyExp (Val v)   = const . pretty $ v
prettyExp (X n)
  | n == 1/2  = const $ text "√" <> text "x"
  | n == 1    = const $ text "x"
  | otherwise = const $ text "x" <> prettySup n
prettyExp (p :+ q)  = infixlP "+" 6 (prettyExp p) (prettyExp q)
prettyExp (v :* p)  = infixlP "*" 7 (const $ pretty v) (prettyExp p)
prettyExp (Recip p) = infixlP "/" 7 (txt "1") (prettyExp p)

{-
instance EXP (P e) where
  (+:) = infixlP "+" 6
  v .* p = infixlP "*" 7 (P (const (pretty v))) p
-}

instance (Fractional a, PrettySup a) => Pretty (Exp a) where
  pretty = prettyP . prettyExp
{-
  pretty (Val v)   = pretty v
  pretty (X n)     = text "x" <> prettySup n
  pretty (p :+ q)  = pretty p <+> text "+" <+> pretty q
  pretty (v :* p)  = pretty v <> parens (pretty p)
  pretty (Recip p) = text "1/" <> parens (pretty p)
-}

type ExpQ = Exp Rational

-- instance Eq Exp where

-- instance Show Exp where

instance Num a => Num (Exp a) where
  fromInteger = Val . fromInteger
  (+) = (:+)
  (*) = undefined
  abs = undefined
  signum = undefined

instance Fractional a => Fractional (Exp a) where
  (/) = undefined
  -- recip = Recip
  recip (Val v)   = Val (recip v)
  recip (X n)     = X (- n)
  recip (p :+ q)  = Recip (p :+ q) -- undefined -- TODO
  recip (v :* p)  = recip v :* recip p
  recip (Recip p) = p
  fromRational = Val . fromRational

zipWith' :: a -> b -> (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' x0 y0 f = go where
  go []     ys     = map (f x0)   ys
  go xs     []     = map (`f` y0) xs
  go (x:xs) (y:ys) = f x y : go xs ys

nrm :: (Integral a, Fractional a) => Exp a -> [a]
nrm (Val n)   = [n]
nrm (X n)
  | n >= 0    = genericReplicate n 0 ++ [1]
  | otherwise = undefined
nrm (x :+ y)  = zipWith' 0 0 (+) (nrm x) (nrm y)
nrm (x :* y)  = map (x *) $ nrm y
nrm (Recip e) = map recip $ nrm e

d :: ExpQ -> ExpQ
d (Val _) = 0
d (X n)
  | n == 0    = 0
  | otherwise = n .* X (n - 1)
d (x :+ y)  = d x + d y
d (x :* y)  = x .* d y
-- d (Recip e) = 

-- d (1/x) = d(x⁻¹) = -1 * x⁻² = -(x²)⁻¹ = -1/x²

-- -2x^2 + 6x + 2
ex1, ex2 :: ExpQ
ex1 = (-2) .* X 2 + 6 .* X 1 + 2
ex2 = (-2) .* X (1/2) + 6 .* X 1 + 2
