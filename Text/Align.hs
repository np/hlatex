-- This module is supposed to provide a mean to align parts
-- of a text/code according to the rule:
--   ``Characters vertically aligned with at least two spaces
--     before them in the source are aligned in output.''
-- Example:
-- foo  :: Bar  -> Baz
--              -> Boo
-- foo  = f  . g
--           . h
--
-- In this example `::' is aligned with `=',
-- `.' are aligned, and `->' are aligned.
--
-- The code is originally from lhs2tex.
-- This module is not finished yet.

module Text.Align where
import Data.List
import Control.Arrow
import Control.Applicative

type Row                      =  Int
type Col                      =  Int

data Pos a                    =  Pos {row :: !Row, col :: !Col, ann :: a}
                                 deriving (Show)

type Text = String
type Line = String
type WoS  = String

align :: Text -> Text
align = unlines . map (intercalate "&") . alignLines . lines

-- | Each line should not contains '\n's
alignLines :: [Line] -> [[String]]
alignLines = map (map concat) . alignWoS . map wordsOrSpaces

alignWoS :: [[WoS]] -> [[[String]]]
alignWoS ls = map (splitAts splitAtCol (findCols ls)) ls

splitAtCol :: Width tok => Col -> [tok] -> ([tok],[tok])
splitAtCol c ts
  | c <= 0 || null ts = ([],ts)
splitAtCol c (t:ts) = t <| splitAtCol (c - width t) ts

(<|)                          :: a -> ([a], b) -> ([a], b)
a <| (as, b)                  =  (a : as, b)

wordsOrSpaces :: Line -> [WoS]
wordsOrSpaces []        = []
wordsOrSpaces (' ':xs)  = (' ':ys) : wordsOrSpaces zs
                        where (ys,zs) = span (==' ') xs
wordsOrSpaces (x:xs)    = (x:ys) : wordsOrSpaces zs
                        where (ys,zs) = break (==' ') xs

findCols :: [[String]] -> [Col]
findCols = nub . sort . concatMap findGaps . number

{-
> lines                         :: [Pos a] -> [[Pos a]]
> lines                         =  split 1
>     where
>     split _   []              =  []
>     split r ts                =  us : split (r + 1) vs
>         where (us, vs)        =  span (\t -> row t <= r) ts
-}


cumSum :: Num a => [a] -> [a]
cumSum = scanl (+) 0

cumDec :: [Int] -> [Int]
cumDec [] = []
cumDec (x:xs) = x : cumDec (map (subtract x) xs)

splitAts :: (Int -> [a] -> ([a],[a])) -> [Int] -> [a] -> [[a]]
{-
splitAts []      xs = [xs]
splitAts (b:bs)  xs = ys : splitAts bs zs
  where (ys,zs) = splitAt b xs
-}
splitAts spltAt = foldr f pure . cumDec
  where f b g = uncurry (:) . second g . spltAt b

-- flagCols :: [Int] -> [Bool]
-- flagCols [] = []
-- flagCols (x:xs) = case compare x 0 of
--   EQ -> True:flagCols (map pred xs)
--   LT -> error "flagCols: unexpected non-starting at 0 increasing list"
--   GT -> False:flagCols (map pred xs)

-- flagCols :: [Int] -> [Bool]
-- flagCols = go 0 where
--   go k [] = []
--   go k (x:xs) = case compare x k of
--     EQ -> True : go (k+1) xs
--     LT -> error "flagCols: unexpected non-starting at 0 increasing list"
--     GT -> replicate False (x-k) ++ go (x+1) xs

{-
findGaps . map (fmap f)
  where f t = head (ann t) == ' ' && width (ann t) >= 2
findGaps :: [Pos (Bool, Bool)] -> [Col]
findGaps (t:xs)
  | fst (ann t)
  = case dropWhile (snd . ann) xs of
      [] -> []
      y:ys -> col y:findGaps ys
findGaps (_:xs) = findGaps xs
findGaps []     = []
-}

class IsSpace a where
  isSpace :: a -> Bool
  isSpace = not . isNotSpace
  isNotSpace :: a -> Bool
  isNotSpace = not . isSpace

instance IsSpace Char where
  isSpace = (==' ') -- Char.isSpace

instance IsSpace a => IsSpace (Pos a) where
  isSpace = isSpace . ann
  isNotSpace = isNotSpace . ann

instance IsSpace a => IsSpace [a] where
  isSpace = all isSpace

class Width a where
  width :: a -> Int

instance Width a => Width (Pos a) where
  width = width . ann

instance Width Char where
  width _ = 1

instance Width a => Width [a] where
  width = sum . map width

--{-# RULES "sum.map(const 1)/length" sum . map (\_ -> 1) = length #-}
{-# RULES "sum/replicate" forall k. sum (replicate k (1::Int)) = k #-}
{-# RULES "map/const" forall k xs. map (\_ -> k) xs = replicate (length xs) k #-}

findGaps :: (Width tok, IsSpace tok) => [Pos tok] -> [Col]
findGaps (t:xs)
  | isSpace t && width t >= 2
  = case dropWhile isSpace xs of
      []   -> []
      y:ys -> col y : findGaps ys
findGaps (_:xs) = findGaps xs
findGaps []     = []

-- currently wrong: should take the width in account
number :: Width a => [[a]] -> [[Pos a]]
number = zipWith numb [0..]
  --where numb row = zipWith (Pos row) <*> (cumSum . map width)
  where numb row x = zipWith (Pos row) (cumSum . map width $ x) x

{-
number                        :: (tok -> String) -> Row -> Col -> [tok] -> [Pos tok]
number str r c []             =  []
number str r c (t : ts)       =  Pos r c t : number str r' c' ts
    where (r', c')            =  count r c (str t)

count                         :: Row -> Col -> String -> (Row, Col)
count r c []                  =  (r, c)
count r c (a : s)
    | a == '\n'               =  count (r + 1) 1       s
    | otherwise               =  count r       (c + 1) s
-}
ex1 = unlines["foo  :: Bar  -> Baz"
             ,"             -> Boo"
             ,"foo  = f  . g"
             ,"          . h"
             ]

