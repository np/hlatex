{-# LANGUAGE QuasiQuotes #-}
module Language.LaTeX.Builder.Beamer

where

import Language.LaTeX.Builder.MonoidUtils
import Language.LaTeX.Types
import qualified Language.LaTeX.Builder as B
import Language.LaTeX.Builder.QQ
import Data.List (intercalate)
import Data.Maybe
import Data.Monoid

{-
pkg :: PackageName
pkg = B.pkgName "beamer"
-}

documentclass :: DocumentClass
documentclass =  OtherDocumentClass "beamer"

frametitle :: LatexItem -> ParItem
frametitle = B.parCmdArg "frametitle"

type TargetName = String
type Label = String

data FrameOpt = Label Label
              | Fragile
  deriving (Eq,Ord) -- Overlays is first

data Overlays = RawOverlays String
              | NoOverlays
  deriving (Eq,Ord)

type BeamerOpt = (String, String)

ppFrameOpt :: FrameOpt -> BeamerOpt
ppFrameOpt (Label lbl) = ("label",lbl)
ppFrameOpt Fragile     = ("fragile","")

ppFrameOpts :: [FrameOpt] -> [Arg LatexItem]
ppFrameOpts = beamerOpts . map ppFrameOpt

ppOverlaysOpt :: Overlays -> Maybe LatexItem
ppOverlaysOpt (RawOverlays s) = Just . B.rawTex . ('<':) . (++">") $ s
ppOverlaysOpt NoOverlays      = Nothing

ppOverlaysArg :: Overlays -> Arg LatexItem
ppOverlaysArg (RawOverlays s) = B.rawArg . ('<':) . (++">") $ s
ppOverlaysArg NoOverlays      = B.noArg

rawOverlays :: String -> Overlays
rawOverlays =  RawOverlays

noOverlays :: Overlays
noOverlays = NoOverlays

label :: Label -> FrameOpt
label = Label

-- more options to add ?
frame :: Overlays -> Overlays -> [FrameOpt] -> LatexItem -> LatexItem -> ParItem  -> ParItem 
frame ov mov fopts title subtitle =
  B.parEnvironmentPar "frame" $ [ ppOverlaysArg ov
                                , maybe B.noArg B.optional $ ppOverlaysOpt mov
                                ] ++ ppFrameOpts fopts ++
                                [ B.mandatory title 
                                , B.mandatory subtitle ]

frameO :: Overlays -> ParItem  -> ParItem 
frameO overlays = B.parEnvironmentPar "frame" [maybe B.noArg B.optional $ ppOverlaysOpt overlays]

example :: ParItem -> ParItem
example = B.parEnvironmentPar "example" []

block :: LatexItem -> ParItem -> ParItem
block title = B.parEnvironmentPar "block" [B.mandatory title]

slide :: LatexItem -> ParItem -> ParItem
slide tit body = frame noOverlays noOverlays [] tit mempty body

slideO :: LatexItem -> Overlays -> ParItem -> ParItem
slideO tit ovs body = frameO ovs (frametitle tit <> body)

fullOv :: Overlays
fullOv = rawOverlays "+-"

ovFromList :: [Int] -> Overlays
ovFromList = rawOverlays . intercalate "," . map show

alert :: LatexItem -> LatexItem
alert = B.latexCmdArg "alert"

-- A shortcut for @itemize' . ppOverlays@
itemize :: Overlays -> [ListItem] -> ParItem
itemize = B.itemize' . ppOverlaysOpt
-- A shortcut for @enumerate' . ppOverlays@
enumerate :: Overlays -> [ListItem] -> ParItem
enumerate = B.enumerate' . ppOverlaysOpt
-- A shortcut for @description' . ppOverlays@
description :: Overlays -> [ListItem] -> ParItem
description = B.description' . ppOverlaysOpt

-- AtBeginSubsection, AtBeginSection

only :: Overlays -> LatexItem -> LatexItem
only ov arg = B.latexCmdArgs "only" [ppOverlaysArg ov, B.mandatory arg]

visible :: Overlays -> LatexItem -> LatexItem
visible ov arg = B.latexCmdArgs "visible" [ppOverlaysArg ov, B.mandatory arg]

alt :: Overlays -> LatexItem -> LatexItem -> LatexItem
alt ov arg1 arg2 = B.latexCmdArgs "alt" [ppOverlaysArg ov, B.mandatory arg1, B.mandatory arg2]

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
  B.latexCmdArgs "hyperlink" [ ppOverlaysArg ov1
                             , B.mandatory (B.rawTex target)
                             , B.mandatory linkText
                             , ppOverlaysArg ov2
                             ] 

againframe :: Overlays -> Overlays -> [FrameOpt] -> Label -> ParItem
againframe ov1 ov2 fopts lbl =
  B.parCmdArgs "againframe" . concat $ [ ppOverlaysArg ov1
                                       , ppOverlaysArg ov2
                                       ] 
                                       : ppFrameOpts fopts : [[B.mandatory (B.rawTex lbl)]]
                                

-- | Disable those litte icons at the bottom right of your presentation.
beamertemplatenavigationsymbolsempty :: PreambleItem
beamertemplatenavigationsymbolsempty = B.preambleCmdArgs "beamertemplatenavigationsymbolsempty" []

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