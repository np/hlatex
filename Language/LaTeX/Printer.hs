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

ppOpts :: [String] -> ShowS
ppOpts []   = text ""
ppOpts opts = brackets (text $ intercalate "," opts)

ppArg :: Arg ShowS -> ShowS
ppArg (Arg k x) = case k of Mandatory   -> braces   x
                            Optional    -> brackets x
                            Coordinate  -> parens   x

ppEnv :: String -> [Arg ShowS] -> ShowS -> ShowS
ppEnv envName args contents =
  text "\\begin" <> braces (text envName) <> mconcat (map ppArg args) <> nl <>
  contents <> text "\n\\end" <> braces (text envName) <> nl

ppCmdNoArg :: String -> ShowS
ppCmdNoArg cmdName = braces (backslash <> text cmdName)

ppCmdArgNB :: String -> ShowS -> ShowS
ppCmdArgNB cmdName arg = backslash <> text cmdName <> braces arg

ppCmdArg :: String -> ShowS -> ShowS
ppCmdArg cmdName arg = braces (ppCmdArgNB cmdName arg)

ppCmdArgs :: String -> [Arg ShowS] -> ShowS
ppCmdArgs cmdName args = backslash <> text cmdName <> mconcat (map ppArg args)


ppDecl :: String -> ShowS
ppDecl declName = backslash <> text declName <> text " " -- or {}

ppDeclOpt :: String -> ShowS -> ShowS
ppDeclOpt declName opt = backslash <> text declName <> brackets opt <> text " " -- or {}

mayBraces :: ShowS -> ShowS
mayBraces = braces
-- mayBraces = id

pp :: Latex -> ShowS
pp (LatexCmd cmdName contents) = ppCmdArg cmdName (pp contents)

pp (LatexCmdArgs cmdName args) = ppCmdArgs cmdName $ map (fmap pp) args

pp (LatexSize size) = ppSize size

pp (LatexKeys keys) = text $ concat $ intersperse "," $ map getKey keys

pp (TexDecl cmdName) = ppDecl cmdName

pp (TexDeclOpt cmdName opt) = ppDeclOpt cmdName $ pp opt

pp (TexCmdNoArg cmdName) = ppCmdNoArg cmdName

pp (TexCmdArg cmdName contents)
 = braces (backslash <> text cmdName <> text " " <> pp contents)

pp (Environment envName args contents) = ppEnv envName (map (fmap pp) args) $ pp contents

pp (LatexParMode pm) = ppParMode pm

pp (RawTex s) = text s

pp (MathsInline m) = text "\\( " <> ppMaths m <> text " \\)"

pp (LatexSaveBin bin) = text $ "hlatexSaveBin" ++ show (unsafeGetSaveBin bin)

pp (TexGroup t) = braces $ pp t

pp (LatexConcat contents) = mconcat $ map pp contents

ppParMode :: ParMode -> ShowS
ppParMode (Para t) = nl <> pp t <> nl <> nl
ppParMode (ParCmdArg cmdName arg) = ppCmdArg cmdName (pp arg)
ppParMode (ParCmdArgs cmdName args) = ppCmdArgs cmdName $ map (fmap pp) args
ppParMode (ParDecl declName) = ppDecl declName
ppParMode (ParDeclOpt declName opt) = ppDeclOpt declName $ pp opt
ppParMode (RawParMode x) = text x
ppParMode (ParGroup p) = braces $ ppParMode p
ppParMode (ParEnvironmentLR envName contents) = ppEnv envName [] $ pp contents
ppParMode (ParEnvironmentPar envName args contents)
  = ppEnv envName (map (fmap pp) args) $ ppParMode contents
ppParMode (DisplayMaths m) = text "\\[ " <> ppMaths m <> text " \\]"
ppParMode (Equation m) = ppEnv "equation" [] $ vcat $ map ppMaths m
ppParMode (Tabular specs rows) =
  ppEnv "tabular" [Arg Mandatory $ text $ map rowSpecChar specs] (ppRows pp rows)
ppParMode (FigureLike name locs body) = ppEnv name [Arg Optional $ text $ map locSpecChar locs] $ ppParMode body

ppParMode (ParConcat contents) = vcat $ map ppParMode contents

ppMaths :: MathsItem -> ShowS
ppMaths (MathsDecl decl) = ppDecl decl
ppMaths (MathsCmd cmd) = ppCmdNoArg cmd
ppMaths (MathsCmdArg cmdName m) = ppCmdArg cmdName (ppMaths m)
ppMaths (MathsCmdArgs cmdName args) = ppCmdArgs cmdName $ map (fmap ppMaths) args
ppMaths (MathsCmdArgNoMath cmdName ss) = ppCmdArg cmdName (mconcat $ map text ss)
ppMaths (RawMaths s) = text s
ppMaths (MathsRat r) | denominator r == 1 = shows (numerator r)
                     | otherwise          = shows (numerator r) <> text " / " <> shows (denominator r)
ppMaths (MathsArray specs rows) = 
  ppEnv "array" [Arg Mandatory $ braces $ text $ map rowSpecChar specs] (ppRows ppMaths rows)
ppMaths (MathsGroup m) = braces $ ppMaths m
ppMaths (MathsConcat ms) = mconcat $ map ppMaths ms
ppMaths (MathsUnOp op m) = text op <> sp <> ppMaths m
ppMaths (MathsBinOp op l r) = parens (ppMaths l <> sp <> text op <> sp <> ppMaths r)
ppMaths (MathsToLR cmd lr) = ppCmdArg cmd (pp lr)
ppMaths (MathsNeedsPackage pkg m) | pkg == "amsmath" = ppMaths m
                                  | otherwise        = error "ppMaths: package system not supported yet"

ppRows :: (a -> ShowS) -> [Row a] -> ShowS
ppRows _ []
  = mempty
ppRows ppCell (Cells cells : rows)
  = (mconcat . intersperse (text " & ") . map ppCell $ cells)
 <> (if null rows then mempty else backslash <> backslash <> nl <> ppRows ppCell rows)
ppRows ppCell (Hline : rows)
  = ppDecl "hline" <> ppRows ppCell rows
ppRows ppCell (Cline c1 c2 : rows)
  = ppCmdArgNB "cline" (text $ show c1 ++ "-" ++ show c2) <> ppRows ppCell rows

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
    SizeCmd cmd        -> ppCmdArgs cmd []
    SizeCmdRatArg cmd r -> ppCmdArg cmd (showr r)
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
ppRoot (Root preamb (Document doc)) = ppPreamble preamb $$ ppEnv "document" [] (ppParMode doc)

