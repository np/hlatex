{-# LANGUAGE QuasiQuotes #-}
module Language.LaTeX.Builder.Beamer

where

import Language.LaTeX.Builder.MonoidUtils
import Language.LaTeX.Types
import qualified Language.LaTeX.Builder.Internal as BI
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

data DocClassOption = Compress
                    | T
                    | Red

compress, t, red :: DocClassOption
compress = Compress
t = T
red = Red

showDocClassOption :: DocClassOption -> String
showDocClassOption Compress  = "compress"
showDocClassOption T         = "t"
showDocClassOption Red       = "red"

documentclasskind :: DocumentClassKind
documentclasskind =  OtherDocumentClassKind "beamer"

beamer :: Maybe LatexLength -> [DocClassOption] -> [LatexItem] -> DocumentClass
beamer msize opts
  = B.documentclass documentclasskind
  . (maybeToList (BI.texLength <$> msize) ++)
  . (map (BI.rawTex . showDocClassOption) opts ++)

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
texFrameOpt (Label lbl)        = ("label",lbl)
texFrameOpt Fragile            = ("fragile","")
texFrameOpt (OtherOption a b)  = (a,b)

texFrameOpts :: [FrameOpt] -> [Arg LatexItem]
texFrameOpts = beamerOpts . map texFrameOpt

showOvInt :: OverlayInt -> ShowS
showOvInt (OvInt i)           = shows i
showOvInt OvPlus              = ('+':)
showOvInt OvDot               = ('.':)
showOvInt (OvPlusOffset off)  = ('+':) . ('(':) . shows off . (')':)

showOverlay :: Overlay -> ShowS
showOverlay (OvSingle i)    = showOvInt i
showOverlay (OvFromTo i j)  = showOvInt i . ('-':) . showOvInt j
showOverlay (OvFrom i)      = showOvInt i . ('-':)

showOverlays :: Overlays -> Maybe String
showOverlays []   = Nothing
showOverlays ovs  = Just . ('<':) . (++">") . showsOv ovs $ []
   where
     showsOv :: Overlays -> ShowS
     showsOv = mconcat . intersperse (',':) . map showOverlay

texOverlaysOpt :: Overlays -> Maybe LatexItem
texOverlaysOpt = fmap BI.rawTex . showOverlays

texOverlaysArg :: Overlays -> Arg LatexItem
texOverlaysArg = maybe BI.noArg BI.rawArg . showOverlays

label :: Label -> FrameOpt
label = Label

-- more options to add ?
frame :: Overlays -> Overlays -> [FrameOpt] -> LatexItem -> LatexItem -> ParItem  -> ParItem
frame ov mov fopts title subtitle =
  {- recent beamer versions
  BI.parEnvironmentPar "frame" $ [ texOverlaysArg ov
                                 , maybe BI.noArg BI.optional $ texOverlaysOpt mov
                                 ] ++ texFrameOpts fopts ++
                                 [ BI.mandatory title
                                 , BI.mandatory subtitle ]
  -}
  BI.parEnvironmentPar "frame" ([ texOverlaysArg ov
                                , maybe BI.noArg BI.optional $ texOverlaysOpt mov
                                ] ++ texFrameOpts fopts) .
     (mapNonEmpty frametitle title ⊕) . (mapNonEmpty framesubtitle subtitle ⊕)

frameO :: Overlays -> ParItem  -> ParItem
frameO overlays = BI.parEnvironmentPar "frame" [maybe BI.noArg BI.optional $ texOverlaysOpt overlays]

example :: ParItem -> ParItem
example = BI.parEnvironmentPar "example" []

theorem :: ParItem -> ParItem
theorem = BI.parEnvironmentPar "theorem" []

block :: LatexItem -> ParItem -> ParItem
block title = BI.parEnvironmentPar "block" [BI.mandatory title]

slide :: LatexItem -> ParItem -> ParItem
slide tit = frame [] [] [] tit ø

slideO :: LatexItem -> Overlays -> ParItem -> ParItem
slideO tit ovs body = frameO ovs (frametitle tit ⊕ body)

frametitle :: LatexItem -> ParItem
frametitle = BI.parCmdArg "frametitle"

framesubtitle :: LatexItem -> ParItem
framesubtitle = BI.parCmdArg "framesubtitle"

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
ovInt i | i > 0      = OvInt i
        | otherwise  = error "ovInt: strictly positive Int expected"

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
alert = BI.latexCmdArg "alert"

-- A shortcut for @itemize . texOverlaysOpt@
itemize :: Overlays -> [ListItem] -> ParItem
itemize = B.itemize . texOverlaysOpt
-- A shortcut for @enumerate . texOverlaysOpt@
enumerate :: Overlays -> [ListItem] -> ParItem
enumerate = B.enumerate . texOverlaysOpt
-- A shortcut for @description . texOverlaysOpt@
description :: Overlays -> [ListItem] -> ParItem
description = B.description . texOverlaysOpt

-- AtBeginSubsection, AtBeginSection

pause :: LatexItem
pause = BI.texCmdNoArg "pause"

pause' :: Maybe Int -> LatexItem
pause' = BI.latexCmdArgs "pause" . maybeToList . fmap (BI.optional . BI.rawTex . show)

only :: Overlays -> LatexItem -> LatexItem
only ov arg = BI.latexCmdArgs "only" [texOverlaysArg ov, BI.mandatory arg]

uncover :: Overlays -> LatexItem -> LatexItem
uncover ov arg = BI.latexCmdArgs "uncover" [texOverlaysArg ov, BI.mandatory arg]

visible :: Overlays -> LatexItem -> LatexItem
visible ov arg = BI.latexCmdArgs "visible" [texOverlaysArg ov, BI.mandatory arg]

invisible :: Overlays -> LatexItem -> LatexItem
invisible ov arg = BI.latexCmdArgs "invisible" [texOverlaysArg ov, BI.mandatory arg]

alt :: Overlays -> LatexItem -> LatexItem -> LatexItem
alt ov arg1 arg2 = BI.latexCmdArgs "alt" [texOverlaysArg ov, BI.mandatory arg1, BI.mandatory arg2]

temporal :: Overlays -> LatexItem -> LatexItem -> LatexItem -> LatexItem
temporal ov arg1 arg2 arg3
  = BI.latexCmdArgs "temporal"  [  texOverlaysArg ov
                                ,  BI.mandatory arg1
                                ,  BI.mandatory arg2
                                ,  BI.mandatory arg3
                                ]

visibleenv :: Overlays -> ParItem -> ParItem
visibleenv ov = BI.parEnvironmentPar "visibleenv" [texOverlaysArg ov]

invisibleenv :: Overlays -> ParItem -> ParItem
invisibleenv ov = BI.parEnvironmentPar "invisibleenv" [texOverlaysArg ov]

uncoverenv :: Overlays -> ParItem -> ParItem
uncoverenv ov = BI.parEnvironmentPar "uncoverenv" [texOverlaysArg ov]

onlyenv :: Overlays -> ParItem -> ParItem
onlyenv ov = BI.parEnvironmentPar "onlyenv" [texOverlaysArg ov]

altenv :: Overlays   -- ^ overlay specification
       -> LatexItem  -- ^ begin text
       -> LatexItem  -- ^ end   text
       -> LatexItem  -- ^ alternate begin text
       -> LatexItem  -- ^ alternate end text
       -> ParItem    -- ^ environment contents
       -> ParItem
altenv ov b e ab ae =
  BI.parEnvironmentPar "altenv"  [  texOverlaysArg ov
                                 ,  BI.mandatory b, BI.mandatory e
                                 ,  BI.mandatory ab, BI.mandatory ae
                                 ]

beamerOpts :: [BeamerOpt] -> [Arg LatexItem]
beamerOpts []  = []
beamerOpts os  = [BI.optional . BI.rawTex . intercalate "," . map f $ os]
  where f (x,y) = x ++ "=" ++ y

beamerPreambleCmdArgs :: String -> [BeamerOpt] -> LatexItem -> PreambleItem
beamerPreambleCmdArgs name opts arg = BI.preambleCmdArgs name (beamerOpts opts ++ [BI.mandatory arg])

usetheme, usefonttheme, useinnertheme, useoutertheme,
  usecolortheme :: [BeamerOpt] -> LatexItem -> PreambleItem

usetheme       = beamerPreambleCmdArgs "usetheme"
usefonttheme   = beamerPreambleCmdArgs "usefonttheme"
useinnertheme  = beamerPreambleCmdArgs "useinnertheme"
useoutertheme  = beamerPreambleCmdArgs "useoutertheme"
usecolortheme  = beamerPreambleCmdArgs "usecolortheme"

{- | Draws a button with the given button text .

  Example: @hyperlink "somewhere" (beamerbutton "Go somewhere")@

  p97 beamer userguide
-}
beamerbutton :: LatexItem -> LatexItem
beamerbutton = BI.latexCmdArg "beamerbutton"

{- | Draws a button with the given button text. Before the text, a small symbol (usually a
     right-pointing arrow) is inserted that indicates that pressing this button will jump
     to another *area* of the presentation.

  Example: @hyperlink "detour" (beamergotobutton "Go to detour")@

  p98 beamer userguide
-}
beamergotobutton :: LatexItem -> LatexItem
beamergotobutton = BI.latexCmdArg "beamergotobutton"

{- | The symbol drawn for this button is usually a double right arrow. Use this button if
     pressing it will skip over a well-defined part of your talk.

  p98 beamer userguide
-}
beamerskipbutton :: LatexItem -> LatexItem
beamerskipbutton = BI.latexCmdArg "beamerskipbutton"

{- | The symbol drawn for this button is usually a left-pointing arrow. Use this button
     if pressing it will return from a detour.

  p98 beamer userguide
-}
beamerreturnbutton :: LatexItem -> LatexItem
beamerreturnbutton = BI.latexCmdArg "beamerreturnbutton"

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
  BI.latexCmdArgs "hyperlink"  [  texOverlaysArg ov1
                               ,  BI.mandatory (BI.rawTex target)
                               ,  BI.mandatory linkText
                               ,  texOverlaysArg ov2
                               ]

againframe :: Overlays -> Overlays -> [FrameOpt] -> Label -> ParItem
againframe ov1 ov2 fopts lbl =
  BI.parCmdArgs "againframe" . concat $  [  texOverlaysArg ov1
                                         ,  texOverlaysArg ov2
                                         ]
                                         : texFrameOpts fopts : [[BI.mandatory (BI.rawTex lbl)]]


-- | Disable those litte icons at the bottom right of your presentation.
beamertemplatenavigationsymbolsempty :: PreambleItem
beamertemplatenavigationsymbolsempty = BI.preambleCmdArgs "beamertemplatenavigationsymbolsempty" []

type TexDimension = LatexLength

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
  TextMarginLeft dim -> BI.rawTex "text margin left=" ⊕ BI.texLength dim
  TextMarginRight dim -> BI.rawTex "text margin right=" ⊕ BI.texLength dim
  SidebarWidthLeft dim -> BI.rawTex "sidebar width left=" ⊕ BI.texLength dim
  SidebarWidthRight dim -> BI.rawTex "sidebar width right=" ⊕ BI.texLength dim
  DescriptionWidth dim -> BI.rawTex "description width=" ⊕ BI.texLength dim
  DescriptionWidthOf txt -> BI.rawTex "description width of=" ⊕ txt
  MiniFrameSize dim -> BI.rawTex "mini frame size=" ⊕ BI.texLength dim
  MiniFrameOffset dim -> BI.rawTex "mini frame offset=" ⊕ BI.texLength dim

setbeamersize :: BeamerSize -> PreambleItem
setbeamersize = BI.preambleCmdArgs "setbeamersize" . pure . BI.mandatory . texBeamerSizeArg

appendix :: ParItem
appendix = BI.parCmdArgs "appendix" []

-- \setbeamercolor*{titlelike}{parent=structure}

data Footline = Footline { authorPercent    :: Percentage
                         , titlePercent     :: Percentage
                         , datePercent      :: Maybe Percentage
                         , showTotalFrames  :: Bool }

defaultFootline :: Footline
defaultFootline = Footline { authorPercent    = 34
                           , titlePercent     = 36
                           , datePercent      = Nothing
                           , showTotalFrames  = True }

footline :: Footline -> PreambleItem
footline Footline{authorPercent=authorp,titlePercent=titlep,datePercent=maydatep,showTotalFrames=stf} =
  let datep = fromMaybe (100 - authorp - titlep) maydatep
      f (Percentage p) = BI.rawPreamble $ show p
      maytotalframes = if stf then [qp| / \inserttotalframenumber|] else ø
  in
  [qp|
        \defbeamertemplate*{footline}{infolines theme without institution}
        {
          \leavevmode%
          \hbox{%
            \begin{beamercolorbox}[wd=.|] ⊕ f authorp ⊕ [qp|\paperwidth,ht=2.25ex,dp=1.125ex,center]{author in head/foot}%
              \usebeamerfont{author in head/foot}
              \insertshortauthor
            \end{beamercolorbox}%
            \begin{beamercolorbox}[wd=.|] ⊕ f titlep ⊕ [qp|\paperwidth,ht=2.25ex,dp=1.125ex,center]{title in head/foot}%
              \usebeamerfont{title in head/foot}
              \insertshorttitle
            \end{beamercolorbox}%
            \begin{beamercolorbox}[wd=.|] ⊕ f datep ⊕ [qp|\paperwidth,ht=2.25ex,dp=1.125ex,right]{date  in head/foot}%
              \usebeamerfont{date in head/foot}
              \insertshortdate{}\hspace*{2em}
              \insertframenumber{}|] ⊕ maytotalframes ⊕ [qp|\hspace*{2ex}
            \end{beamercolorbox}}%
          \vskip0pt%
        }
  |]
