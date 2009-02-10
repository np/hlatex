module Language.LaTeX.Printer where

import Data.Monoid
import Data.List (intercalate, intersperse)

import Language.LaTeX.Types
import Language.LaTeX.Builder (amp)
import Language.LaTeX.Internal

text :: String -> ShowS
text = showString

braces, brackets :: ShowS -> ShowS
braces x = text "{" <> x <> text "}"
brackets x = text "[" <> x <> text "]"

nl, backslash :: ShowS
backslash = text "\\"
nl = text "\n"

($$) :: ShowS -> ShowS -> ShowS
($$) x y = x <> nl <> y

vcat :: [ShowS] -> ShowS
vcat = mconcat . intersperse nl

ppOpts :: [String] -> ShowS
ppOpts opts | null opts = text ""
            | otherwise = brackets (text $ intercalate "," opts)

ppEnv :: String -> [String] -> ShowS -> ShowS
ppEnv envName opts contents =
  text "\\begin" <> braces (text envName) <> ppOpts opts <> nl <>
  contents <> text "\n\\end" <> braces (text envName) <> nl

mayBraces :: ShowS -> ShowS
mayBraces = braces
-- mayBraces = id

pp :: Latex -> ShowS
pp (LatexCmd cmdName contents)
 = mayBraces (backslash <> text cmdName <> braces (pp contents))

pp (LatexCmdArgs cmdName args)
 = mayBraces (backslash <> text cmdName <> mconcat (map (braces . pp) args))

pp (LatexSize size) = text $ showSize size

pp (TexCmd cmdName) = mayBraces (backslash <> text cmdName)

pp (TexCmdArg cmdName contents)
 = braces (backslash <> text cmdName <> text " " <> pp contents)

pp (Environment envName opts contents) = ppEnv envName opts $ pp contents

pp (RawTex s) = text s

pp (MathsBlock m) = text "\\[ " <> ppMaths m <> text " \\]"
pp (MathsInline m) = text "\\( " <> ppMaths m <> text " \\)"

pp (Tabular rows) =
  ppEnv "tabular" [] (mconcat (intersperse (backslash <> backslash) $ map ppRow rows))

pp (TexGroup t) = braces $ pp t

pp (LatexConcat contents) = vcat $ map pp contents -- TODO horiz

ppMaths :: MathsItem -> ShowS
ppMaths (MathsCmd cmd) = mayBraces (backslash <> text (mathsCmdName cmd))
ppMaths (MathsCmdArg cmdName m) = mayBraces (backslash<>text cmdName<>braces (ppMaths m))
ppMaths (MathsCmdArgNoMath cmdName ss) = mayBraces (backslash <> text cmdName <> braces (mconcat $ map text ss))
ppMaths (RawMaths s) = text s
ppMaths (MathsInt i) = shows i
ppMaths (MathsGroup m) = braces $ ppMaths m
ppMaths (MathsConcat ms) = mconcat $ map ppMaths ms

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
ppRoot (Root preamb doc) = ppPreamble preamb $$ pp doc

