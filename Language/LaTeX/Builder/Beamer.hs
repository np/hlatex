{-# LANGUAGE QuasiQuotes #-}
module Language.LaTeX.Builder.Beamer

where

import Language.LaTeX.Builder.MonoidUtils
import Language.LaTeX.Types
import qualified Language.LaTeX.Builder as B
import Language.LaTeX.Builder.QQ
import Data.List (sort, intercalate)
import Data.Maybe

{-
pkg :: PackageName
pkg = B.pkgName "beamer"
-}

documentclass :: DocumentClass
documentclass =  OtherDocumentClass "beamer"

frametitle :: LatexItem -> ParItem
frametitle = B.parCmdArg "frametitle"

data FrameOpt = Overlays Overlays
              | Fragile
  deriving (Eq,Ord) -- Overlays is first

data Overlays = RawOverlays String
  deriving (Eq,Ord)

type BeamerOpt = (String, String)

ppFrameOpt :: FrameOpt -> [Arg LatexItem]
ppFrameOpt (Overlays overlays) = [ppOverlaysArg overlays]
ppFrameOpt Fragile             = [B.optional (B.rawTex "fragile")]

ppOverlays :: Overlays -> LatexItem
ppOverlays (RawOverlays s) = B.rawTex . ('<':) . (++">") $ s

ppOverlaysArg :: Overlays -> Arg LatexItem
ppOverlaysArg (RawOverlays s) = B.rawArg . ('<':) . (++">") $ s

rawOverlays :: String -> Overlays
rawOverlays =  RawOverlays

-- more options to add ?
frame' :: [FrameOpt] -> ParItem  -> ParItem 
frame' opts = B.parEnvironmentPar "frame" (ppFrameOpt =<< sort opts)

frameO :: Overlays -> ParItem  -> ParItem 
frameO overlays = B.parEnvironmentPar "frame" [B.optional (ppOverlays overlays)]

frame :: ParItem -> ParItem 
frame = B.parEnvironmentPar "frame" []

example :: ParItem -> ParItem
example = B.parEnvironmentPar "example" []

block :: LatexItem -> ParItem -> ParItem
block title = B.parEnvironmentPar "block" [B.mandatory title]

slide :: LatexItem -> ParItem -> ParItem
slide tit body = frame (frametitle tit <> body)

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
itemize = B.itemize' . ppOverlays
-- A shortcut for @enumerate' . ppOverlays@
enumerate :: Overlays -> [ListItem] -> ParItem
enumerate = B.enumerate' . ppOverlays
-- A shortcut for @description' . ppOverlays@
description :: Overlays -> [ListItem] -> ParItem
description = B.description' . ppOverlays

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