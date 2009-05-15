{-# LANGUAGE QuasiQuotes #-}
module Language.LaTeX.Builder.Beamer

where

import Language.LaTeX.Builder.MonoidUtils
import Language.LaTeX.Types
import qualified Language.LaTeX.Builder as B
import Language.LaTeX.Builder.QQ
import Data.List (sort)
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

ppFrameOpt :: FrameOpt -> [Arg LatexItem]
ppFrameOpt (Overlays overlays) = ppOverlays overlays
ppFrameOpt Fragile             = [B.optional (B.rawTex "fragile")]

ppOverlays :: Overlays -> [Arg LatexItem]
ppOverlays (RawOverlays s) = [B.optional . B.hstring $ s]

rawOverlays :: String -> Overlays
rawOverlays =  RawOverlays

-- more options to add ?
frame' :: [FrameOpt] -> ParItem  -> ParItem 
frame' opts = B.parEnvironmentPar "frame" ({-B.packageDependency pkg : -}(ppFrameOpt =<< sort opts))

frameO :: Overlays -> ParItem  -> ParItem 
frameO overlays = B.parEnvironmentPar "frame" ({-B.packageDependency pkg : -}ppOverlays overlays)

frame :: ParItem -> ParItem 
frame = B.parEnvironmentPar "frame" [{-B.packageDependency pkg-}]

example :: ParItem -> ParItem
example = B.parEnvironmentPar "example" []

block :: LatexItem -> ParItem -> ParItem
block title = B.parEnvironmentPar "block" [B.mandatory title]

slide :: LatexItem -> ParItem -> ParItem
slide tit body = frame (frametitle tit <> body)

alert :: LatexItem -> LatexItem
alert = B.latexCmdArg "alert"

-- AtBeginSubsection, AtBeginSection

only :: LatexItem -> LatexItem
only = B.latexCmdArg "only"

usetheme, usefonttheme :: LatexItem -> PreambleItem
usetheme = B.preambleCmdArg "usetheme"
usefonttheme = B.preambleCmdArg "usefonttheme"
-- \setbeamercolor*{titlelike}{parent=structure}
-- setbeamercolorStar = 

data Footline = Footline { author_percent :: Percentage
                         , title_percent  :: Percentage
                         , date_percent   :: Maybe Percentage }  

defaultFootline :: Footline
defaultFootline = Footline { author_percent = 34
                           , title_percent  = 36  
                           , date_percent   = Nothing } 

footline :: Footline -> PreambleItem
footline Footline{author_percent=authorp,title_percent=titlep,date_percent=maydatep} =
  let datep = fromMaybe (100 - authorp - titlep) maydatep
      f (Percentage p) = show p
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
              \insertframenumber{} / \inserttotalframenumber\hspace*{2ex}
            \end{beamercolorbox}}%
          \vskip0pt%
        }
  |]