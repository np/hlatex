module Language.LaTeX.Printer where

import Data.Monoid
import Data.List (intercalate, intersperse)

import Language.LaTeX.Types
import Language.LaTeX.Builder (amp)

infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend

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

ppOpts opts | null opts = text ""
            | otherwise = brackets (text $ intercalate "," opts)

ppEnv :: String -> [String] -> ShowS -> ShowS
ppEnv envName opts contents =
  text "\\begin" <> braces (text envName) <> ppOpts opts <> nl <>
  contents <> text "\n\\end" <> braces (text envName) <> nl

mayBraces = braces
-- mayBraces = id

pp :: Latex -> ShowS
pp (LatexCmd cmdName contents)
 = mayBraces (backslash <> text cmdName <> braces (mconcat (map pp contents)))

pp (LatexCmdArgs cmdName args)
 = mayBraces (backslash <> text cmdName <> mconcat (map (braces . mconcat . map pp) args))

pp (LatexSize size) = text $ showSize size

pp (TexCmd cmdName) = mayBraces (backslash <> text cmdName)

pp (TexCmdArg cmdName contents)
 = braces (backslash <> text cmdName <> text " " <> mconcat (map pp contents))

pp (Environment envName opts contents) = ppEnv envName opts (vcat $ map pp contents)

pp (RawTex s) = text s

pp (MathsBlock ms) = text "\\[ "<>mconcat (map ppMaths ms)<>text " \\]"
pp (MathsInline ms) = text "\\( "<>mconcat (map ppMaths ms)<>text " \\)"

pp (Tabular rows) =
  ppEnv "tabular" [] (mconcat (intersperse (backslash <> backslash) $ map ppRow rows))

pp (TexGroup ts) = braces $ mconcat $ map pp ts

ppMaths :: MathsItem -> ShowS
ppMaths (MathsCmd cmd) = mayBraces (backslash <> text (mathsCmdName cmd))
ppMaths (MathsCmdArg cmdName ms) = mayBraces (backslash<>text cmdName<>braces (mconcat (map ppMaths ms)))
ppMaths (MathsCmdArgNoMath cmdName ss) = mayBraces (backslash <> text cmdName <> braces (mconcat $ map text ss))
ppMaths (RawMaths s) = text s
ppMaths (MathsInt i) = shows i
ppMaths (MathsGroup ms) = braces $ mconcat $ map ppMaths ms
ppMaths (MathsConcat ms) = mconcat $ map ppMaths ms

ppRow :: Row -> ShowS
ppRow = mconcat . map pp . intersperse amp . getRaw

ppPreambule :: Preambule -> ShowS
ppPreambule (PreambuleCmd s) = backslash <> text s
ppPreambule (PreambuleCmdArg cmdName args)
  = backslash <> text cmdName <> braces (mconcat $ map pp args)
ppPreambule (PreambuleCmdArgWithOpts cmdName opts args)
  = backslash <> text cmdName <> ppOpts opts <> braces (mconcat $ map pp args)

ppRoot :: Root -> ShowS
ppRoot (Root preamb doc) = vcat (map ppPreambule preamb) $$ vcat (map pp doc)

