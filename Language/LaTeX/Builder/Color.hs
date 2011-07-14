{-# LANGUAGE OverloadedStrings #-}
module Language.LaTeX.Builder.Color
  (
  -- * The type of colors
   Color
  -- * Coloring commands
  ,pagecolor
  ,color
  ,textcolor
  ,colorbox
  ,fcolorbox
  ,preamblecolor
  ,normalcolor
  -- * Making colors
  ,named, rgb, cymk, gray
  -- * Predefined colors
  ,red, green, blue, black, white, cyan, magenta, yellow, orange
  -- * Package name
  ,pkg)
where

import Data.String
import Language.LaTeX.Types
import qualified Language.LaTeX.Builder.Internal as BI
import Control.Monad.Error (throwError)

data Color = BaseColor String
           | NamedColor String
           | RGB Rational Rational Rational
           | CYMK Rational Rational Rational Rational
           | Gray Rational

colorArgs :: Color -> [Arg AnyItem]
colorArgs = (BI.packageDependency pkg:) . f
  where f (BaseColor n)  = [man n]
        f (NamedColor n) = [opt "named", man n]
        f (RGB r g b)    = model "rgb" [r,g,b]
        f (CYMK c y m k) = model "cymk" [c,y,m,k]
        f (Gray g)       = model "gray" [g]
        opt = BI.optionalLatexItem . fromString
        man = BI.mandatoryLatexItem . fromString
        model name cs    = [opt name,
                            BI.mandatoryList . map BI.num $ cs]

red, green, blue, black, white, cyan, magenta, yellow :: Color
[red, green, blue, black, white, cyan, magenta, yellow] =
   map BaseColor ["red", "green", "blue", "black", "white", "cyan", "magenta", "yellow"]

orange :: Color
orange = rgb 1 0.5 0

pkg :: PackageName
pkg = BI.pkgName "color"

named :: String -> Color
named = NamedColor

rgb :: Rational -> Rational -> Rational -> Color
rgb = RGB

cymk :: Rational -> Rational -> Rational -> Rational -> Color
cymk = CYMK

gray :: Rational -> Color
gray = Gray

-- | 'pagecolor' sets the background colour for the current and following pages
pagecolor :: Color -> ParItem
pagecolor = BI.parCmdArgs "pagecolor" . colorArgs

-- | 'color' is a declaration to switch to setting text in the given colour
color :: Color -> TexDecl
color = BI.texDecl' "color" . colorArgs

-- | 'textcolor' sets the text of its argument in the given colour
textcolor :: Color -> LatexItem -> LatexItem
textcolor c x = BI.latexCmdAnyArgs "textcolor" (colorArgs c ++ [BI.mandatoryLatexItem $ x])

-- | 'colorbox' sets its argument in a box with the given colour as background
colorbox :: Color -> LatexItem -> LatexItem
colorbox c x = BI.latexCmdAnyArgs "colorbox" (colorArgs c ++ [BI.mandatoryLatexItem $ x])

-- | @fcolorbox c1 c2 text@ is like 'colorbox', with a frame of @c1@ around a box
--   of background colour @c2@.
--   For example, @fcolorbox red green "Text"@ sets `Text' in the current text colour
--   on a green background with a red frame.
--
--   The two specifications must either both be defined ones, or both use the same
--   model, which is given only once, this limitation only make sense in LaTeX
--   parlance but is helpful here to understand why this function can fail.
fcolorbox :: Color -> Color -> LatexItem -> LatexItem
fcolorbox c1 c2 x =
  let args =
       case (colorArgs c1, colorArgs c2) of
         ([p, m1, a1], [_, m2, a2]) | m1 == m2 -> [p, m1, a1, a2]
         ([p, a1], [_, a2]) -> [p, a1, a2]
         _ -> [BI.mandatory (AnyItem $ throwError "fcolorbox: arguments must be either of the same model,\
                                       \ or be defined colors.")]
  in
  BI.latexCmdAnyArgs "fcolorbox" (args ++ [BI.mandatoryLatexItem x])

-- | Like 'color' but usable in the preamble.
preamblecolor :: Color -> PreambleItem
preamblecolor = BI.preambleCmdArgs "color" . colorArgs

-- | 'normalcolor' switches to the colour that was active at the end of the preamble.
--   Thus placing a 'color' declaration in the preamble can change the standard colour
--   for the whole document. This is the equivalent to 'normalfont' for font selection.
normalcolor :: TexDecl
normalcolor = BI.texDecl' "normalcolor" [BI.packageDependency pkg]
