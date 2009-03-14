{-# LANGUAGE OverloadedStrings #-}
module Language.LaTeX.Builder.Color
  (pagecolor
  ,color
  ,textcolor
  ,colorbox
  ,fcolorbox
  ,preamblecolor
  ,normalcolor
  ,named, rgb, cymk, gray
  ,red, green, blue, black, white, cyan, magenta, yellow, orange
  ,pkg)
where

import Data.String
import Data.List
import Data.Monoid
import Language.LaTeX.Types
import qualified Language.LaTeX.Builder as B
import Control.Monad.Error (throwError)

data Color = BaseColor String
           | NamedColor String
           | RGB Rational Rational Rational
           | CYMK Rational Rational Rational Rational
           | Gray Rational

colorArgs :: Color -> [Arg LatexItem]
colorArgs = (B.packageDependency pkg:) . f
  where f (BaseColor n)  = [B.mandatory (fromString n)]
        f (NamedColor n) = [B.optional "named", B.mandatory (fromString n)]
        f (RGB r g b)    = model "rgb" [r,g,b]
        f (CYMK c y m k) = model "cymk" [c,y,m,k]
        f (Gray g)       = model "gray" [g]
        model name cs    = [B.optional name
                           ,B.mandatory (mconcat (intersperse "," (map B.num cs)))]

red, green, blue, black, white, cyan, magenta, yellow :: Color
[red, green, blue, black, white, cyan, magenta, yellow] =
   map BaseColor ["red", "green", "blue", "black", "white", "cyan", "magenta", "yellow"]

orange :: Color
orange = rgb 1 0.5 0

pkg :: PackageName
pkg = B.pkgName "color"

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
pagecolor = B.parCmdArgs "pagecolor" . colorArgs

-- | 'color' is a declaration to switch to setting text in the given colour
color :: Color -> TexDecl
color = B.texDecl' "color" . colorArgs

-- | 'textcolor' sets the text of its argument in the given colour
textcolor :: Color -> LatexItem -> LatexItem
textcolor c x = B.latexCmdArgs "textcolor" (colorArgs c ++ [B.mandatory x])

-- | 'colorbox' sets its argument in a box with the given colour as background
colorbox :: Color -> LatexItem -> LatexItem
colorbox c x = B.latexCmdArgs "colorbox" (colorArgs c ++ [B.mandatory x])

-- | @fcolorbox c1 c2 text@ is like @colorbox@, with a frame of 'c1' around a box
--   of background colour 'c2'.
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
         _ -> [B.mandatory (throwError "fcolorbox: arguments must be either of the same model,\
                                       \ or be defined colors.")]
  in
  B.latexCmdArgs "fcolorbox" (args ++ [B.mandatory x])

-- | Like 'color' but usable in the preamble.
preamblecolor :: Color -> PreambleItem
preamblecolor = B.preambleCmdArgs "color" . colorArgs

-- | 'normalcolor' switches to the colour that was active at the end of the preamble.
--   Thus placing a @color@ declaration in the preamble can change the standard colour
--   for the whole document. This is the equivalent to 'normalfont' for font selection.
normalcolor :: TexDecl
normalcolor = B.texDecl' "normalcolor" [B.packageDependency pkg]
