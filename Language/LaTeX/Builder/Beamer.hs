{-# LANGUAGE QuasiQuotes #-}
module Language.LaTeX.Builder.Beamer

where

import Language.LaTeX.Builder.MonoidUtils
import Language.LaTeX.Types
import qualified Language.LaTeX.Builder as B
import Language.LaTeX.Builder.QQ
import Control.Applicative
import Data.List (intersperse,intercalate)
import Data.Maybe
import Data.Monoid

{-
pkg :: PackageName
pkg = B.pkgName "beamer"
-}

documentclass :: DocumentClass
documentclass =  OtherDocumentClass "beamer"

type TargetName = String
type Label = String

data FrameOpt = Label Label
              | Fragile
              | OtherOption String String
  deriving (Eq,Ord)

data OverlayInt = OvInt Int
                | OvPlus
                | OvPlusOffset Int
                | OvDot
  deriving (Eq,Ord)

-- | Only overlay actions are not supported currently.
data Overlay = OvSingle OverlayInt
             | OvFromTo OverlayInt OverlayInt
             | OvFrom OverlayInt
  deriving (Eq,Ord)

type Overlays = [Overlay]

type BeamerOpt = (String, String)

texFrameOpt :: FrameOpt -> BeamerOpt
texFrameOpt (Label lbl) = ("label",lbl)
texFrameOpt Fragile     = ("fragile","")
texFrameOpt (OtherOption a b) = (a,b)

texFrameOpts :: [FrameOpt] -> [Arg LatexItem]
texFrameOpts = beamerOpts . map texFrameOpt

showOvInt :: OverlayInt -> ShowS
showOvInt (OvInt i)          = shows i
showOvInt OvPlus             = ('+':)
showOvInt OvDot              = ('.':)
showOvInt (OvPlusOffset off) = ('+':) . ('(':) . shows off . (')':)

showOverlay :: Overlay -> ShowS
showOverlay (OvSingle i)   = showOvInt i
showOverlay (OvFromTo i j) = showOvInt i . ('-':) . showOvInt j
showOverlay (OvFrom i)     = showOvInt i . ('-':)

showOverlays :: Overlays -> Maybe String
showOverlays []  = Nothing
showOverlays ovs = Just . ('<':) . (++">") . showsOv ovs $ []
   where showsOv :: Overlays -> ShowS
         showsOv = mconcat . intersperse (',':) . map showOverlay

texOverlaysOpt :: Overlays -> Maybe LatexItem
texOverlaysOpt = fmap B.rawTex . showOverlays

texOverlaysArg :: Overlays -> Arg LatexItem
texOverlaysArg = maybe B.noArg B.rawArg . showOverlays

label :: Label -> FrameOpt
label = Label

-- more options to add ?
frame :: Overlays -> Overlays -> [FrameOpt] -> LatexItem -> LatexItem -> ParItem  -> ParItem
frame ov mov fopts title subtitle =
  {- recent beamer versions
  B.parEnvironmentPar "frame" $ [ texOverlaysArg ov
                                , maybe B.noArg B.optional $ texOverlaysOpt mov
                                ] ++ texFrameOpts fopts ++
                                [ B.mandatory title
                                , B.mandatory subtitle ]
  -}
  B.parEnvironmentPar "frame" ([ texOverlaysArg ov
                               , maybe B.noArg B.optional $ texOverlaysOpt mov
                               ] ++ texFrameOpts fopts) .
     (frametitle title<>) . (framesubtitle subtitle<>)

frameO :: Overlays -> ParItem  -> ParItem
frameO overlays = B.parEnvironmentPar "frame" [maybe B.noArg B.optional $ texOverlaysOpt overlays]

example :: ParItem -> ParItem
example = B.parEnvironmentPar "example" []

block :: LatexItem -> ParItem -> ParItem
block title = B.parEnvironmentPar "block" [B.mandatory title]

slide :: LatexItem -> ParItem -> ParItem
slide tit body = frame [] [] [] tit mempty body

slideO :: LatexItem -> Overlays -> ParItem -> ParItem
slideO tit ovs body = frameO ovs (frametitle tit <> body)

frametitle :: LatexItem -> ParItem
frametitle = B.parCmdArg "frametitle"

framesubtitle :: LatexItem -> ParItem
framesubtitle = B.parCmdArg "framesubtitle"

-- | All overlays counting from the given argument (like in @<1->@).
ovFrom :: OverlayInt -> Overlay
ovFrom = OvFrom

-- | All overlays between the given arguments (like in @<1-3>@).
ovFromTo :: OverlayInt -> OverlayInt -> Overlay
ovFromTo = OvFromTo

-- | The single overlay (like in @<1>@).
ovSingle :: OverlayInt -> Overlay
ovSingle = OvSingle

-- | Lift a strictly positive 'Int' to an 'OverlayInt'
ovInt :: Int -> OverlayInt
ovInt i | i > 0     = OvInt i
        | otherwise = error "ovInt: strictly positive Int expected"

{- | The '+' incremental overlay specification (like in @<+->@).

     Beamer User Guide at 8.6.4 Incremental Specifications -}
ovPlus :: OverlayInt
ovPlus = OvPlus

{- | The '.' incremental overlay specification (like in @<.->@).

     Beamer User Guide at 8.6.4 Incremental Specifications -}
ovDot :: OverlayInt
ovDot = OvDot

-- | Handy shortcut for @[ovFrom ovPlus]@ aka @<+->@.
ovIncr :: Overlays
ovIncr = [ovFrom ovPlus]

-- | Handy lifting for a list of strictly positive integers.
ovInts :: [Int] -> Overlays
ovInts = map (ovSingle . ovInt)

alert :: LatexItem -> LatexItem
alert = B.latexCmdArg "alert"

-- A shortcut for @itemize' . texOverlaysOpt@
itemize :: Overlays -> [ListItem] -> ParItem
itemize = B.itemize' . texOverlaysOpt
-- A shortcut for @enumerate' . texOverlaysOpt@
enumerate :: Overlays -> [ListItem] -> ParItem
enumerate = B.enumerate' . texOverlaysOpt
-- A shortcut for @description' . texOverlaysOpt@
description :: Overlays -> [ListItem] -> ParItem
description = B.description' . texOverlaysOpt

-- AtBeginSubsection, AtBeginSection

only :: Overlays -> LatexItem -> LatexItem
only ov arg = B.latexCmdArgs "only" [texOverlaysArg ov, B.mandatory arg]

visible :: Overlays -> LatexItem -> LatexItem
visible ov arg = B.latexCmdArgs "visible" [texOverlaysArg ov, B.mandatory arg]

alt :: Overlays -> LatexItem -> LatexItem -> LatexItem
alt ov arg1 arg2 = B.latexCmdArgs "alt" [texOverlaysArg ov, B.mandatory arg1, B.mandatory arg2]

beamerOpts :: [BeamerOpt] -> [Arg LatexItem]
beamerOpts [] = []
beamerOpts os = [B.optional . B.rawTex . intercalate "," . map f $ os]
  where f (x,y) = x ++ "=" ++ y

beamerPreambleCmdArgs :: String -> [BeamerOpt] -> LatexItem -> PreambleItem
beamerPreambleCmdArgs name opts arg = B.preambleCmdArgs name (beamerOpts opts ++ [B.mandatory arg])

usetheme, usefonttheme, useinnertheme, useoutertheme,
  usecolortheme :: [BeamerOpt] -> LatexItem -> PreambleItem

usetheme      = beamerPreambleCmdArgs "usetheme"
usefonttheme  = beamerPreambleCmdArgs "usefonttheme"
useinnertheme = beamerPreambleCmdArgs "useinnertheme"
useoutertheme = beamerPreambleCmdArgs "useoutertheme"
usecolortheme = beamerPreambleCmdArgs "usecolortheme"

{- | Draws a button with the given button text .

  Example: @hyperlink "somewhere" (beamerbutton "Go somewhere")@

  p97 beamer userguide
-}
beamerbutton :: LatexItem -> LatexItem
beamerbutton = B.latexCmdArg "beamerbutton"

{- | Draws a button with the given button text. Before the text, a small symbol (usually a
     right-pointing arrow) is inserted that indicates that pressing this button will jump
     to another *area* of the presentation.

  Example: @hyperlink "detour" (beamergotobutton "Go to detour")@

  p98 beamer userguide
-}
beamergotobutton :: LatexItem -> LatexItem
beamergotobutton = B.latexCmdArg "beamergotobutton"

{- | The symbol drawn for this button is usually a double right arrow. Use this button if
     pressing it will skip over a well-defined part of your talk.

  p98 beamer userguide
-}
beamerskipbutton :: LatexItem -> LatexItem
beamerskipbutton = B.latexCmdArg "beamerskipbutton"

{- | The symbol drawn for this button is usually a left-pointing arrow. Use this button
     if pressing it will return from a detour.

  p98 beamer userguide
-}
beamerreturnbutton :: LatexItem -> LatexItem
beamerreturnbutton = B.latexCmdArg "beamerreturnbutton"

{- |
  Only one overlay specification may be given. The link text is typeset in the
  usual way. If you click anywhere on this text, you will jump to the slide on
  which the \hypertarget command was used with the parameter target name . If an
  overlay specification is present, the hyperlink (including the link text) is
  completely suppressed on the non-specified slides.

  p99 beamer userguide
-}
hyperlink :: Overlays -> TargetName -> LatexItem -> Overlays -> LatexItem
hyperlink ov1 target linkText ov2 =
  B.latexCmdArgs "hyperlink" [ texOverlaysArg ov1
                             , B.mandatory (B.rawTex target)
                             , B.mandatory linkText
                             , texOverlaysArg ov2
                             ]

againframe :: Overlays -> Overlays -> [FrameOpt] -> Label -> ParItem
againframe ov1 ov2 fopts lbl =
  B.parCmdArgs "againframe" . concat $ [ texOverlaysArg ov1
                                       , texOverlaysArg ov2
                                       ]
                                       : texFrameOpts fopts : [[B.mandatory (B.rawTex lbl)]]


-- | Disable those litte icons at the bottom right of your presentation.
beamertemplatenavigationsymbolsempty :: PreambleItem
beamertemplatenavigationsymbolsempty = B.preambleCmdArgs "beamertemplatenavigationsymbolsempty" []

type TexDimension = LatexSize

data BeamerSize
  = TextMarginLeft TexDimension
    -- ^ sets a new left margin. This excludes the left sidebar. Thus,
    -- it is the distance between the right edge of the left sidebar and the left edge of the text.
  | TextMarginRight TexDimension
    -- ^ sets a new right margin.
  | SidebarWidthLeft TexDimension
    -- ^ sets the size of the left sidebar. Currently, this command
    -- should be given before a shading is installed for the sidebar canvas.
  | SidebarWidthRight TexDimension
    -- ^ sets the size of the right sidebar.
  | DescriptionWidth TexDimension
    -- ^ sets the default width of description labels, see Beamer User Guide Section 11.1.
  | DescriptionWidthOf LatexItem
    -- ^ sets the default width of description labels to the width of the
    -- text, see Section 11.1.
  | MiniFrameSize TexDimension
    -- ^ sets the size of mini frames in a navigation bar. When two
    -- mini frame icons are shown alongside each other, their left end points are
    -- 'TexDimension' far apart.
  | MiniFrameOffset TexDimension
    -- ^ set an additional vertical offset that is added to the mini
    -- frame size when arranging mini frames vertically.

texBeamerSizeArg :: BeamerSize -> LatexItem
texBeamerSizeArg bs = case bs of
  TextMarginLeft dim -> B.rawTex "text margin left=" <> B.size dim
  TextMarginRight dim -> B.rawTex "text margin right=" <> B.size dim
  SidebarWidthLeft dim -> B.rawTex "sidebar width left=" <> B.size dim
  SidebarWidthRight dim -> B.rawTex "sidebar width right=" <> B.size dim
  DescriptionWidth dim -> B.rawTex "description width=" <> B.size dim
  DescriptionWidthOf txt -> B.rawTex "description width of=" <> txt
  MiniFrameSize dim -> B.rawTex "mini frame size=" <> B.size dim
  MiniFrameOffset dim -> B.rawTex "mini frame offset=" <> B.size dim

setbeamersize :: BeamerSize -> PreambleItem
setbeamersize = B.preambleCmdArgs "setbeamersize" . pure . B.mandatory . texBeamerSizeArg

appendix :: ParItem
appendix = B.parCmdArgs "appendix" []

-- \setbeamercolor*{titlelike}{parent=structure}
-- setbeamercolorStar =

data Footline = Footline { author_percent :: Percentage
                         , title_percent  :: Percentage
                         , date_percent   :: Maybe Percentage
                         , show_total_frames :: Bool }

defaultFootline :: Footline
defaultFootline = Footline { author_percent = 34
                           , title_percent  = 36
                           , date_percent   = Nothing
                           , show_total_frames = True }

footline :: Footline -> PreambleItem
footline Footline{author_percent=authorp,title_percent=titlep,date_percent=maydatep,show_total_frames=stf} =
  let datep = fromMaybe (100 - authorp - titlep) maydatep
      f (Percentage p) = show p
      maytotalframes = if stf then [$str| / \inserttotalframenumber|] else ""
  in
  B.rawPreamble $
  [$str|
        \defbeamertemplate*{footline}{infolines theme without institution}
        {
          \leavevmode%
          \hbox{%
            \begin{beamercolorbox}[wd=.|] <> f authorp <> [$str|\paperwidth,ht=2.25ex,dp=1.125ex,center]{author in head/foot}%
              \usebeamerfont{author in head/foot}
              \insertshortauthor
            \end{beamercolorbox}%
            \begin{beamercolorbox}[wd=.|] <> f titlep <> [$str|\paperwidth,ht=2.25ex,dp=1.125ex,center]{title in head/foot}%
              \usebeamerfont{title in head/foot}
              \insertshorttitle
            \end{beamercolorbox}%
            \begin{beamercolorbox}[wd=.|] <> f datep <> [$str|\paperwidth,ht=2.25ex,dp=1.125ex,right]{date  in head/foot}%
              \usebeamerfont{date in head/foot}
              \insertshortdate{}\hspace*{2em}
              \insertframenumber{}|] <> maytotalframes <> [$str|\hspace*{2ex}
            \end{beamercolorbox}}%
          \vskip0pt%
        }
  |]