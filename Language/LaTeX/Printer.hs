module Language.LaTeX.Printer where

import Data.Monoid
import Data.List (intercalate, intersperse)

import Language.LaTeX.Types
import Language.LaTeX.Builder (amp)
import Language.LaTeX.Internal

text :: String -> ShowS
text = showString

between opening closing x = text opening <> x <> text closing

braces, brackets :: ShowS -> ShowS
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

ppEnv :: String -> [String] -> ShowS -> ShowS
ppEnv envName opts contents =
  text "\\begin" <> braces (text envName) <> ppOpts opts <> nl <>
  contents <> text "\n\\end" <> braces (text envName) <> nl

ppLatexCmdArgs :: String -> [Opts] -> [ShowS] -> ShowS
ppLatexCmdArgs cmdName optss args
 = mayBraces (backslash <> text cmdName <> mconcat (map ppOpts optss) <> mconcat (map braces args))

ppDecl :: String -> Opts -> ShowS
ppDecl declName opts = backslash <> text declName <> ppOpts opts <> text " " -- or {}

mayBraces :: ShowS -> ShowS
mayBraces = braces
-- mayBraces = id

pp :: Latex -> ShowS
pp (LatexCmd cmdName contents)
 = mayBraces (backslash <> text cmdName <> braces (pp contents))

pp (LatexCmdArgs cmdName optss args) = ppLatexCmdArgs cmdName optss $ map pp args

pp (LatexSize size) = text $ showSize size

pp (TexDecl cmdName opts) = ppDecl cmdName opts

pp (TexCmdNoArg cmdName) = ppLatexCmdArgs cmdName [] []

pp (TexCmdArg cmdName contents)
 = braces (backslash <> text cmdName <> text " " <> pp contents)

pp (Environment envName opts contents) = ppEnv envName opts $ pp contents

pp (RawTex s) = text s

pp (MathsInline m) = text "\\( " <> ppMaths m <> text " \\)"

pp (TexGroup t) = braces $ pp t

pp (LatexConcat contents) = mconcat $ map pp contents

ppParMode :: ParMode -> ShowS
ppParMode (Para t) = nl <> pp t <> nl <> nl
ppParMode (ParCmdArg cmdName arg) = backslash <> text cmdName <> braces (pp arg)
ppParMode (ParCmdArgs cmdName optss args) = ppLatexCmdArgs cmdName optss $ map pp args
ppParMode (ParDecl declName opts) = ppDecl declName opts
ppParMode (RawParMode x) = text x
ppParMode (ParGroup p) = braces $ ppParMode p
ppParMode (ParEnvironmentLR envName opts contents) = ppEnv envName opts $ pp contents
ppParMode (ParEnvironmentPar envName opts contents) = ppEnv envName opts $ ppParMode contents
ppParMode (DisplayMaths m) = text "\\[ " <> ppMaths m <> text " \\]"
ppParMode (Equation m) = ppEnv "equation" [] (vcat $ map ppMaths m)
ppParMode (Tabular rows) =
  ppEnv "tabular" [] (mconcat (intersperse (backslash <> backslash) $ map ppRow rows))
ppParMode (ParConcat contents) = vcat $ map ppParMode contents


ppMaths :: MathsItem -> ShowS
ppMaths (MathsDecl decl opts) = ppDecl decl opts
ppMaths (MathsCmd cmd) = mayBraces (backslash <> text (mathsCmdName cmd))
ppMaths (MathsCmdArg cmdName m) = mayBraces (backslash<>text cmdName<>braces (ppMaths m))
ppMaths (MathsCmdArgs cmdName optss args) = ppLatexCmdArgs cmdName optss $ map ppMaths args
ppMaths (MathsCmdArgNoMath cmdName ss) = mayBraces (backslash <> text cmdName <> braces (mconcat $ map text ss))
ppMaths (RawMaths s) = text s
ppMaths (MathsInt i) = shows i
ppMaths (MathsGroup m) = braces $ ppMaths m
ppMaths (MathsConcat ms) = mconcat $ map ppMaths ms
ppMaths (MathsBinOp op l r) = parens (ppMaths l <> sp <> text op <> sp <> ppMaths r)

ppRow :: Row -> ShowS
ppRow = mconcat . map pp . intersperse amp . getRow

ppPreamble :: Preamble -> ShowS
ppPreamble (PreambleCmd s) = backslash <> text s
ppPreamble (PreambleCmdArg cmdName arg)
  = backslash <> text cmdName <> braces (pp arg)
ppPreamble (PreambleCmdArgWithOpts cmdName opts arg)
  = backslash <> text cmdName <> ppOpts opts <> braces (pp arg)
ppPreamble (PreambleConcat ps) = vcat $ map ppPreamble ps

ppRoot :: Root -> ShowS
ppRoot (Root preamb (Document doc)) = ppPreamble preamb $$ ppEnv "document" [] (ppParMode doc)

