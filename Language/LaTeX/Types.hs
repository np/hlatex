{-# LANGUAGE TemplateHaskell #-}
module Language.LaTeX.Types where

import Prelude hiding (and)
import Data.Monoid
import Data.List hiding (and)
import Data.Char
import Data.Ratio ((%), numerator, denominator)
import GHC.Float (formatRealFloat, FFFormat(FFFixed))
import Control.Monad.Writer
import Control.Applicative
import Control.Arrow
import Language.Haskell.TH
import Language.LaTeX.Data

{-
   data MathsCmd = Alpha | Beta | ... | Leftrightarrow | OtherMathsCmd String
     deriving (Eq)
   mathsCmdName ::  MathsCmd -> String
   mathsCmdName Alpha = "alpha"
   ...
 -}
$(
  let sigValD n ty e = [sigD n ty, valD (varP n) (normalB e) []]
      upperMCName name = mkName ("MC"++name)
      mkCon (name, _) = normalC (upperMCName name) []
      other = normalC (mkName "OtherMathsCmd") [liftM2 (,) notStrict [t| String |]]
      mathsCmd = mkName "MathsCmd"
      mkClause (name, cmd) = match (conP (upperMCName name) []) (normalB (stringE cmd)) []
  in
  sequence
  (dataD (return []) mathsCmd [] (map mkCon mathsCmds ++ [other]) [''Eq]
  :sigValD (mkName "mathsCmdName") (arrowT `appT` conT mathsCmd `appT` [t| String |])
     [| \x -> $(caseE [| x |] (map mkClause mathsCmds)) |]
  )
 )

data Root = Root Preamble Latex

data DocumentClass = Article
                   | Book
                   | Report
                   | Letter
                   | OtherDocumentClass

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
           | LatexCmdArgs String [Latex]
           | TexCmd String
           | TexCmdArg String Latex
           | Environment String [String] Latex
           | MathsBlock MathsItem
           | MathsInline MathsItem
           | Tabular [Row]
           | LatexSize LatexSize
           | RawTex String
           | TexGroup Latex
           | LatexConcat [Latex]

instance Monoid Latex where
  mempty  = LatexConcat []
  LatexConcat xs `mappend` LatexConcat ys = LatexConcat (xs ++ ys)
  LatexConcat xs `mappend` y              = LatexConcat (xs ++ [y])
  x              `mappend` LatexConcat ys = LatexConcat (x : ys)
  x              `mappend` y              = LatexConcat [x, y]

data MathsItem = MathsCmd MathsCmd -- String
               | MathsCmdArg String MathsItem
               | MathsCmdArgNoMath String [String]
               | RawMaths String
               | MathsInt Int
               | MathsGroup MathsItem
               | MathsConcat [MathsItem]

instance Monoid MathsItem where
  mempty  = MathsConcat []
  MathsConcat xs `mappend` MathsConcat ys = MathsConcat (xs ++ ys)
  MathsConcat xs `mappend` y              = MathsConcat (xs ++ [y])
  x              `mappend` MathsConcat ys = MathsConcat (x : ys)
  x              `mappend` y              = MathsConcat [x, y]

data LatexSize = Pt Rational
               | Em Rational
               | Cm Rational

data LatexPaper = A4paper

newtype Row = Row { getRow :: [Latex] }

newtype LatexItem = LatexItem { getLatexItem :: Latex }

type LatexM = Writer Latex ()

showSize :: LatexSize -> String
showSize s =
  case s of
    Cm i -> showr i ++ "cm" 
    Em i -> showr i ++ "em" 
    Pt i -> showr i ++ "pt" 
  where showr r | denominator r == 1 = show $ numerator r
                | otherwise          = formatRealFloat FFFixed (Just 2) (fromRational r :: Double)

showPaper :: LatexPaper -> String
showPaper A4paper = "a4paper"

showDocumentClass :: DocumentClass -> String
showDocumentClass Article = "article"
showDocumentClass Book    = "book"
showDocumentClass Report  = "report"
showDocumentClass Letter  = "letter"