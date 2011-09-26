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
  ,named, rgb, cmyk, gray, html, rgb256
  -- * Predefined colors
  ,red, green, blue, black, white, cyan, magenta, yellow, orange
  -- * Package name
  ,pkg)
where

import Data.String
import Data.Word
import Language.LaTeX.Types
import qualified Language.LaTeX.Builder.Internal as BI
import Control.Monad.Error (throwError)

-- Some HTML docs at http://en.wikibooks.org/wiki/LaTeX/Colors

data Color = BaseColor String
           | NamedColor String
           | RGB Rational Rational Rational
           | RGB256 Word8 Word8 Word8
           | HTML String
           | CMYK Rational Rational Rational Rational
           | Gray Rational

colorArgs :: Color -> [Arg AnyItem]
colorArgs = (BI.packageDependency pkg:) . f
  where f (BaseColor n)  = [man n]
        f (NamedColor n) = [opt "named", man n]
        f (RGB r g b)    = model "rgb" (checkRatios [r,g,b])
        f (RGB256 r g b) = model "RGB" [r,g,b]
        f (CMYK c m y k) = model "cmyk" (checkRatios [c,m,y,k])
        f (Gray g)       = model "gray" (checkRatios [g])
        f (HTML s)       = [opt "HTML", man . checkHTML $ s]
        opt = BI.optionalLatexItem . fromString
        man = BI.mandatoryLatexItem . fromString
        model name cs    = [opt name,
                            BI.mandatoryList . map BI.num $ cs]
        check msg p x | p x       = x
                      | otherwise = error msg
        checkHTML = check "Not a HTML color (6 hex digits)" (\s -> length s == 6 && all (`elem` (['0'..'9']++['a'..'F']++['A'..'F'])) s)
        checkRatios = check "Not a rational number between 0 and 1" (all checkRatio)
        checkRatio x = x >= 0 && x <= 1

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

cmyk :: Rational -> Rational -> Rational -> Rational -> Color
cmyk = CMYK

gray :: Rational -> Color
gray = Gray

rgb256 :: Word8 -> Word8 -> Word8 -> Color
rgb256 = RGB256

html :: String -> Color
html = HTML

-- | 'pagecolor' sets the background colour for the current and following pages
pagecolor :: Color -> ParItem
pagecolor = BI.parCmdArgs "pagecolor" . colorArgs

-- | 'color' is a declaration to switch to setting text in the given colour
color :: Color -> TexDecl
color = BI.texDecl' "color" . colorArgs

-- | 'textcolor' sets the text of its argument in the given colour
textcolor :: Color -> LatexItem -> LatexItem
textcolor c x = BI.latexCmdAnyArgs "textcolor" (colorArgs c ++ [BI.mandatoryLatexItem x])

-- | 'colorbox' sets its argument in a box with the given colour as background
colorbox :: Color -> LatexItem -> LatexItem
colorbox c x = BI.latexCmdAnyArgs "colorbox" (colorArgs c ++ [BI.mandatoryLatexItem x])

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
