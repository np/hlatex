module Language.LaTeX.Builder.Graphics
  (
  -- * The main command
  includegraphics
  -- * The options
  ,IncludeGraphicsOpts(..)
  -- * The locations
  ,Loc, c, center, t, top, l, left, r, right, b, bottom, ba, baseline, tr, topRight
  ,tl, topLeft, bl, bottomLeft, br, bottomRight, baselineLeft, baselineRight
  -- * The package name
  , pkg)
where

import Language.LaTeX.Types hiding (Loc)
import Language.LaTeX.Builder (parCmdArgs, size, bool, coord, packageDependency,
                               rat, rawTex, optional, mandatory)
import Language.LaTeX.Builder.MonoidUtils ((<>))
import Control.Arrow ((***))
import Data.Maybe
import Data.String
import Data.List
import Data.Monoid

data Loc = C   -- ^ Center
         | T   -- ^ Top
         | L   -- ^ Left
         | R   -- ^ Right
         | B   -- ^ Bottom
         | Ba  -- ^ Baseline
         | TR  -- ^ Top right
         | TL  -- ^ Top left
         | BL  -- ^ Bottom left (default)
         | BR  -- ^ Bottom right
         | BaL -- ^ Baseline left
         | BaR -- ^ Baseline right
  deriving (Eq)

c, center, t, top, l, left, r, right, b, bottom, ba, baseline, tr, topRight,
  tl, topLeft, bl, bottomLeft, br, bottomRight, baselineLeft, baselineRight :: Loc
c = C
center = C
t = T
top = T
l = L
left = L
r = R
right = R
b = B
bottom = B
ba = Ba
baseline = Ba
tr = TR
topRight = TR
tl = TL
topLeft = TL
bl = BL
bottomLeft = BL
br = BR
bottomRight = BR
baselineLeft = BaL
baselineRight = BaR

showGrLoc :: Loc -> String
showGrLoc loc = case loc of
  C -> "c"
  T -> "t"
  L -> "l"
  R -> "r"
  B -> "b"
  Ba -> "B"
  TR -> "tr"
  TL -> "tl"
  BL -> "bl"
  BR -> "br"
  BaL -> "Bl"
  BaR -> "Br"

data IncludeGraphicsOpts = IncludeGraphicsOpts
  { scale  :: Rational
    -- ^ the number by which the figure size should be magnified over its natural size
  , width  :: Maybe LatexSize
    -- ^ specifies the width to which the figure should be scaled to; if height not given,
    -- it is scaled with the same factor as the width
  , height :: Maybe LatexSize
    -- ^ specifies the height to which the figure should be scaled to; if width is not given,
    -- it is scaled with the same factor as the height
  , totalheight :: Maybe LatexSize
    -- ^ like height but specifies the height plus depth of the figure; should always be used
    -- in place of height if the figure has been otated
  , keepaspectratio :: Bool
    -- ^ if both height and width are specified, this flag ensures that the original height/width
    -- ratio remains unchanged; the figure will not exceed either of the given dimensions
  , angle :: Rational
    -- ^ the angle by which the figure is to be rotated counterclockwise, in degrees; any height
    -- or width specifications coming before this key are also rotated, so that the height becomes
    -- the width, while the width becomes either the height (positive angle) or depth (negative angle)
  , origin :: Loc
    -- ^ determines the point about which the rotation occurs; default is bl for bottom left
    -- corner; also possible are c for center, t for top, r for right, and B for baseline;
    -- any sensible combination, such as tr, is allowed
  , draft :: Bool
    -- ^ like the draft package option but applied to the one graphics file; the figure is not
    -- imported, but rather a framed box of the correct size is printed containing the name of
    -- the file
  , clip :: Bool
    -- ^ suppresses the printing of any graphic outside the bounding box
  , bb :: Maybe (Coord, Coord)
    -- ^ ((llx, lly) (urx, ury)); enters the coordinates of the bounding box manually,
    -- if they are missing or incorrect in the graphics file, or to be deliberately altered;
    -- the specifications are four lengths separated by blanks; units may be given, but if
    -- omitted, big points (bp) are assumed
  , viewport :: Maybe (Coord, Coord)
    -- ^ ((llx, lly), (urx, ury)); specifies the bounding box but relative to the lower left
    -- corner of the existing one; useful for correcting the bounding box, or (with clip) to
    -- select only a portion of the whole figure
  , trim :: Maybe (Coord, Coord)
    -- ^ ((dllx, dlly), (durx, dury)); reduces the existing bounding box by the amounts specified
  , hiresbb :: Bool
    -- ^ like the hiresbb package option but applied to the one graphics file; reads bounding box
    -- information from the %%HiResBoundingBox line in the graphics file. 
  }

pkg :: PackageName
pkg = PkgName "graphicx"

-- | @includegraphics fopts fp@
-- The @fopts@ function will receive the defaults options and should modify options
-- to suit your needs.
--
-- This function is generally used like this:
-- @
-- includegraphics (\\o -> o{ \<opt\> = \<exp\> ... }) fp
-- @
includegraphics :: (IncludeGraphicsOpts -> IncludeGraphicsOpts) -> FilePath -> ParItem
includegraphics f fp =
   parCmdArgs "includegraphics" $ opt ++ [packageDependency pkg, mandatory $ fromString fp]
  where h (name, item) = rawTex (name ++ "=") <> item
        opts = map h $ includeGraphicsOpts $ f defaultOpts
        opt | null opts = []
            | otherwise = [optional $ mconcat $ intersperse (rawTex ",") opts]

defaultOpts :: IncludeGraphicsOpts
defaultOpts = IncludeGraphicsOpts
  { scale = 1
  , width = Nothing
  , height = Nothing
  , totalheight = Nothing
  , keepaspectratio = False
  , angle = 0
  , origin = baselineLeft
  , draft = False
  , clip = False
  , bb = Nothing
  , viewport = Nothing
  , trim = Nothing
  , hiresbb = False -- I think that's the default.
  }

includeGraphicsOpts :: IncludeGraphicsOpts -> [(String, LatexItem)]
includeGraphicsOpts o =
  catMaybes [ f "scale" scale rat
            , f "width" width (size . fromJust)
            , f "height" height (size . fromJust)
            , f "totalheight" totalheight (size . fromJust)
            , f "keepaspectratio" keepaspectratio bool
            , f "angle" angle rat
            , f "origin" origin (rawTex . showGrLoc)
            , f "draft" draft bool
            , f "clip" clip bool
            , f "bb" bb maybeCoords
            , f "viewport" viewport maybeCoords
            , f "trim" trim maybeCoords
            , f "hiresbb" hiresbb bool
            ]
  where f :: Eq a => String -> (IncludeGraphicsOpts -> a) -> (a -> LatexItem) -> Maybe (String, LatexItem)
        f name proj toLatexItem
            | proj defaultOpts == proj o = Nothing
            | otherwise                  = Just (name, toLatexItem $ proj o)
        maybeCoords = g . (coord *** coord) . fromJust
        g (x, y) = x <> rawTex " " <> y
