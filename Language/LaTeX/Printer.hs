module Language.LaTeX.Printer where

import Data.Monoid
import Data.Maybe
import Data.List (intercalate, intersperse)
import Data.Ratio (numerator, denominator)
import GHC.Float (formatRealFloat, FFFormat(FFFixed))

import Language.LaTeX.Types
import Language.LaTeX.Internal

text :: String -> ShowS
text = showString

between :: String -> String -> ShowS -> ShowS
between opening closing x = text opening <> x <> text closing


braces, brackets, parens :: ShowS -> ShowS
braces   = between "{" "}"
brackets = between "[" "]"
parens   = between "(" ")"


nl, backslash, sp :: ShowS
backslash = text "\\"
nl = text "\n"
sp = text " "

($$) :: ShowS -> ShowS -> ShowS
($$) x y = x <> nl <> y

vcat :: [ShowS] -> ShowS
vcat = mconcat . intersperse nl

ppOpts :: Opts -> ShowS
ppOpts []   = text ""
ppOpts opts = brackets (text $ intercalate "," opts)

ppEnv :: String -> Opts -> Maybe ShowS -> ShowS -> ShowS
ppEnv envName opts mayarg contents =
  text "\\begin" <> braces (text envName) <> ppOpts opts <> fromMaybe mempty mayarg <> nl <>
  contents <> text "\n\\end" <> braces (text envName) <> nl

ppCmd :: String -> [Opts] -> [ShowS] -> ShowS
ppCmd cmdName optss args
 = {-mayBraces -}(backslash <> text cmdName <> mconcat (map ppOpts optss) <> mconcat (map braces args))

ppDecl :: String -> Opts -> ShowS
ppDecl declName opts = backslash <> text declName <> ppOpts opts <> text " " -- or {}

mayBraces :: ShowS -> ShowS
mayBraces = braces
-- mayBraces = id

ppArg :: (a -> ShowS) -> (Bool, a) -> ShowS
ppArg ppElt (b, x) | b         = braces $ ppElt x
                   | otherwise = parens $ ppElt x

pp :: Latex -> ShowS
pp (LatexCmd cmdName contents)
 = mayBraces (backslash <> text cmdName <> braces (pp contents))

pp (LatexCmdArgs cmdName args) = ppCmd cmdName [] $ map (ppArg pp) args

pp (LatexSize size) = ppSize size

pp (LatexKeys keys) = text $ concat $ intersperse "," $ map getKey keys

pp (TexDecl cmdName opts) = ppDecl cmdName opts

pp (TexCmdNoArg cmdName) = ppCmd cmdName [] []

pp (TexCmdArg cmdName contents)
 = braces (backslash <> text cmdName <> text " " <> pp contents)

pp (Environment envName opts contents) = ppEnv envName opts Nothing $ pp contents

pp (LatexParMode pm) = ppParMode pm

pp (RawTex s) = text s

pp (MathsInline m) = text "\\( " <> ppMaths m <> text " \\)"

pp (LatexSaveBin bin) = text $ "hlatexSaveBin" ++ show (unsafeGetSaveBin bin)

pp (TexGroup t) = braces $ pp t

pp (LatexConcat contents) = mconcat $ map pp contents

ppParMode :: ParMode -> ShowS
ppParMode (Para t) = nl <> pp t <> nl <> nl
ppParMode (ParCmdArg cmdName arg) = backslash <> text cmdName <> braces (pp arg)
ppParMode (ParCmdArgs cmdName optss args) = ppCmd cmdName optss $ map pp args
ppParMode (ParDecl declName opts) = ppDecl declName opts
ppParMode (RawParMode x) = text x
ppParMode (ParGroup p) = braces $ ppParMode p
ppParMode (ParEnvironmentLR envName opts contents) = ppEnv envName opts Nothing $ pp contents
ppParMode (ParEnvironmentPar envName opts contents) = ppEnv envName opts Nothing $ ppParMode contents
ppParMode (DisplayMaths m) = text "\\[ " <> ppMaths m <> text " \\]"
ppParMode (Equation m) = ppEnv "equation" [] Nothing (vcat $ map ppMaths m)
ppParMode (Tabular specs rows) =
  ppEnv "tabular" [] (Just $ braces $ text $ map rowSpecChar specs) (ppRows pp rows)
ppParMode (FigureLike name locs body) = ppEnv name [map locSpecChar locs] Nothing $ ppParMode body

ppParMode (ParConcat contents) = vcat $ map ppParMode contents

ppMaths :: MathsItem -> ShowS
ppMaths (MathsDecl decl opts) = ppDecl decl opts
ppMaths (MathsCmd cmd) = mayBraces (backslash <> text cmd)
ppMaths (MathsCmdArg cmdName m) = mayBraces (backslash<>text cmdName<>braces (ppMaths m))
ppMaths (MathsCmdArgs cmdName optss args) = ppCmd cmdName optss $ map ppMaths args
ppMaths (MathsCmdArgNoMath cmdName ss) = mayBraces (backslash <> text cmdName <> braces (mconcat $ map text ss))
ppMaths (RawMaths s) = text s
ppMaths (MathsRat r) | denominator r == 1 = shows (numerator r)
                     | otherwise          = shows (numerator r) <> text " / " <> shows (denominator r)
ppMaths (MathsArray specs rows) = 
  ppEnv "array" [] (Just $ braces $ text $ map rowSpecChar specs) (ppRows ppMaths rows)
ppMaths (MathsGroup m) = braces $ ppMaths m
ppMaths (MathsConcat ms) = mconcat $ map ppMaths ms
ppMaths (MathsUnOp op m) = text op <> sp <> ppMaths m
ppMaths (MathsBinOp op l r) = parens (ppMaths l <> sp <> text op <> sp <> ppMaths r)
ppMaths (MathsToLR cmd lr) = ppCmd cmd [] [pp lr]
ppMaths (MathsNeedsPackage pkg m) | pkg == "amsmath" = ppMaths m
                                  | otherwise        = error "ppMaths: package system not supported yet"

ppRows :: (a -> ShowS) -> [Row a] -> ShowS
ppRows _ []
  = mempty
ppRows ppCell (Cells cells : rows)
  = (mconcat . intersperse (text " & ") . map ppCell $ cells)
 <> (if null rows then mempty else backslash <> backslash <> nl <> ppRows ppCell rows)
ppRows ppCell (Hline : rows)
  = ppDecl "hline" [] <> ppRows ppCell rows
ppRows ppCell (Cline c1 c2 : rows)
  = ppCmd "cline" [] [text $ show c1 ++ "-" ++ show c2] <> ppRows ppCell rows

ppSize :: LatexSize -> ShowS
ppSize s =
  case s of
    Cm r -> showr r <> text "cm" 
    Mm r -> showr r <> text "mm" 
    Em r -> showr r <> text "em" 
    Ex r -> showr r <> text "ex" 
    Pt r -> showr r <> text "pt" 
    Pc r -> showr r <> text "pc" 
    In r -> showr r <> text "in"
    SizeInt i          -> shows i
    SizeUnOp op s'     -> text op <> sp <> ppSize s'
    SizeBinOp op s1 s2 -> parens (ppSize s1 <> sp <> text op <> sp <> ppSize s2)
    SizeCmd cmd        -> ppCmd cmd [] []
    SizeCmdRatArg cmd r -> ppCmd cmd [] [showr r]
  where showr r | denominator r == 1 = shows $ numerator r
                | otherwise          = text $ formatRealFloat FFFixed (Just 2) (fromRational r :: Double)

ppPreamble :: Preamble -> ShowS
ppPreamble (PreambleCmd s) = backslash <> text s
ppPreamble (PreambleCmdArg cmdName arg)
  = backslash <> text cmdName <> braces (pp arg)
ppPreamble (PreambleCmdArgWithOpts cmdName opts arg)
  = backslash <> text cmdName <> ppOpts opts <> braces (pp arg)
ppPreamble (PreambleConcat ps) = vcat $ map ppPreamble ps

ppRoot :: Root -> ShowS
ppRoot (Root preamb (Document doc)) = ppPreamble preamb $$ ppEnv "document" [] Nothing (ppParMode doc)

