module Language.LaTeX.Printer where

import Data.Monoid
import Data.Maybe
import Data.List (intersperse)
import Data.Ratio (numerator, denominator)
import Data.Generics.UniplateStr (universe)
import Data.Generics.Biplate (universeBi)
import Data.Foldable (foldMap)
import Data.Set (Set)
import Data.Char
import qualified Data.Set as Set
import GHC.Float (formatRealFloat, FFFormat(FFFixed))

import Language.LaTeX.Types
import Language.LaTeX.Builder.MonoidUtils

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
vcat = mconcat . intersperse (nl <> nl)

ppArg :: Arg ShowS -> ShowS
ppArg NoArg             = id
ppArg (Mandatory x)     = braces x
ppArg (Optional  x)     = brackets x
ppArg (Optionals xs)    = brackets $ mconcat $ intersperse (text ",") xs
ppArg (Coordinates x y) = parens (x <> text " " <> y)
ppArg (RawArg x)        = text x
ppArg (PackageDependency _) = id

ppEnv :: String -> [Arg ShowS] -> ShowS -> ShowS
ppEnv envName args contents =
  backslash<>begin<>braces envNameS<>mconcat (map ppArg args)
 $$
  contents
 $$
  backslash<>end<>braces envNameS

  where envNameS = text envName
        begin    = text "begin"
        end      = text "end"

ppCmdNoArg :: String -> ShowS
ppCmdNoArg cmdName = braces (backslash <> text cmdName)

ppCmdArgNB :: String -> ShowS -> ShowS
ppCmdArgNB cmdName arg = backslash <> text cmdName <> braces arg

ppCmdArg :: String -> ShowS -> ShowS
ppCmdArg cmdName arg = braces (ppCmdArgNB cmdName arg)

ppCmdArgs :: String -> [Arg ShowS] -> ShowS
ppCmdArgs cmdName args = backslash <> text cmdName <> mconcat (map ppArg args)


ppDecl :: String -> ShowS -> ShowS
ppDecl declName declArgs = backslash <> text declName <> declArgs <> text " " -- or {}

ppTexDecl :: TexDcl -> ShowS
ppTexDecl (TexDcl declName declArgs) = ppDecl declName (foldMap (ppArg . fmap pp) declArgs)

ppMathDecl :: MathDcl -> ShowS
ppMathDecl (MathDcl declName) = ppDecl declName mempty

pp :: LatexItm -> ShowS

pp (LatexCmdArgs cmdName args) = ppCmdArgs cmdName $ map (fmap pp) args

pp (LatexLength texLength) = ppTexLength texLength

pp (LatexCoord (Coord x y)) = ppTexLength x <> text " " <> ppTexLength y

pp (LatexKeys keys) = text $ concat $ intersperse "," $ map getKey keys

pp (TexDecls decls) = foldMap ppTexDecl decls

pp (TexCmdNoArg cmdName) = ppCmdNoArg cmdName

pp (TexCmdArg cmdName contents)
 = braces (backslash <> text cmdName <> text " " <> pp contents)

pp (Environment envName args contents) = ppEnv envName (map (fmap pp) args) $ pp contents

pp (LatexParMode pm) = ppParMode pm

pp (RawTex s) = text s

-- One produce $...$ since \(...\) is fragile
pp (MathInline m) = text "$ " <> ppMath m <> text " $"

pp (LatexSaveBin bin) = text $ "\\hlatexSaveBin" ++ (map enc . show $ unsafeGetSaveBin bin)
  where enc i = chr (ord 'a' + digitToInt i) -- hackish but numbers are prohibited

pp (TexGroup t) = braces $ pp t

pp (LatexConcat contents) = mconcat $ map pp contents

pp (LatexNote note t) = ppNote note pp t

ppParMode :: ParItm -> ShowS
ppParMode (Para t) = pp t
ppParMode (ParCmdArgs cmdName args) = ppCmdArgs cmdName $ map (fmap pp) args
ppParMode (RawParMode x) = text x
ppParMode (ParGroup p) = braces $ ppParMode p
ppParMode (ParEnvironmentLR envName contents) = ppEnv envName [] $ pp contents
ppParMode (ParEnvironmentPar envName args contents)
  = ppEnv envName (map (fmap pp) args) $ ppParMode contents
ppParMode (DisplayMath m) = text "\\[ " <> ppMath m <> text " \\]"
ppParMode (Equation m) = ppEnv "equation" [] $ vcat $ map ppMath m
ppParMode (Tabular specs rows) =
  ppEnv "tabular" [Mandatory $ mconcat $ map (ppRowSpec . fmap pp) specs] (ppRows pp rows)
ppParMode (FigureLike name locs body) = ppEnv name [Optional $ text $ map locSpecChar locs] $ ppParMode body

ppParMode (ParConcat contents) = vcat $ map ppParMode contents
ppParMode (ParNote note t) = ppNote note ppParMode t

ppMath :: MathItm -> ShowS
ppMath (MathDecls decls) = foldMap ppMathDecl decls
ppMath (MathCmdArgs cmdName args) = ppCmdArgs cmdName $ map (fmap ppMath) args
ppMath (RawMath s) = text s
ppMath (MathRat r) | denominator r == 1 = shows (numerator r)
                     | otherwise          = shows (numerator r) <> text " / " <> shows (denominator r)
ppMath (MathArray specs rows) = 
  ppEnv "array" [Mandatory $ mconcat $ map (ppRowSpec . fmap ppMath) specs] (ppRows ppMath rows)
ppMath (MathGroup m) = braces $ ppMath m
ppMath (MathConcat ms) = mconcat $ map ppMath ms
ppMath (MathUnOp op m) = text op <> sp <> ppMath m
ppMath (MathBinOp op l r) = parens (ppMath l <> sp <> text op <> sp <> ppMath r)
ppMath (MathToLR cmdName args) = ppCmdArgs cmdName $ map (fmap pp) args
ppMath (MathNote note m) = ppNote note ppMath m

ppRowSpec :: RowSpec ShowS -> ShowS
ppRowSpec Rc        = text "c"
ppRowSpec Rl        = text "l"
ppRowSpec Rr        = text "r"
ppRowSpec Rvline    = text "|"
ppRowSpec (Rtext x) = text "@" <> braces x


ppRows :: (a -> ShowS) -> [Row a] -> ShowS
ppRows _ []
  = mempty
ppRows ppCell (Cells cells : rows)
  = (mconcat . intersperse (text " & ") . map ppCell $ cells)
 <> (if null rows then mempty else backslash <> backslash $$ ppRows ppCell rows)
ppRows ppCell (Hline : rows)
  = backslash <> text "hline " <> ppRows ppCell rows
ppRows ppCell (Cline c1 c2 : rows)
  = ppCmdArgNB "cline" (text $ show c1 ++ "-" ++ show c2) <> ppRows ppCell rows

unitName :: TexUnit -> String
unitName u =
  case u of
    Cm -> "cm" 
    Mm -> "mm" 
    Em -> "em" 
    Ex -> "ex" 
    Pt -> "pt" 
    Pc -> "pc" 
    In -> "in"
    Sp -> "sp"
    Bp -> "bp"
    Dd -> "dd"
    Cc -> "cc"
    Mu -> "mu"

ppTexLength :: LatexLength -> ShowS
ppTexLength s =
  case s of
    LengthCmd cmd          -> ppCmdArgs cmd []
    LengthCmdRatArg  cmd r -> ppCmdArg cmd (showr r)
    LengthScaledBy _ (LengthScaledBy _ _) ->
      error "broken invariant: nested LengthScaledBy"
    LengthScaledBy r l     -> showr r <> ppTexLength l
    LengthCst munit r      -> showr r <> foldMap (text . unitName) munit
  where showr r | denominator r == 1 = shows $ numerator r
                | otherwise          = text $ formatRealFloat FFFixed (Just 2) (fromRational r :: Double)

ppPreamble :: PreambleItm -> ShowS
ppPreamble (PreambleCmd s) = backslash <> text s
ppPreamble (PreambleCmdArgs cmdName args) = ppCmdArgs cmdName $ map (fmap pp) args
ppPreamble (PreambleConcat ps) = vcat $ map ppPreamble ps
ppPreamble (Usepackage pkg args)
  = ppCmdArgs "usepackage" (map (fmap pp) args ++ [Mandatory (text $ getPkgName pkg)])
ppPreamble (RawPreamble raw) = text raw
ppPreamble (PreambleNote note p) = ppNote note ppPreamble p

ppNote :: Note -> (a -> ShowS) -> a -> ShowS
ppNote note ppElt elt =  nl' <> mconcat (map ((<>nl') . text) . lines . showNote $ note) <> ppElt elt <> nl'
  where nl' = text "%\n%"
        showNote (TextNote s) = show s
        showNote (IntNote  i) = show i
        showNote (LocNote  loc) = showLoc loc

showLoc :: Loc -> String
showLoc (Loc fp line char) = unwords [fp, ":", show line, ":", show char]

showPaper :: LatexPaper -> String
showPaper A4paper = "a4paper"

showDocClassKind :: DocumentClassKind -> String
showDocClassKind Article = "article"
showDocClassKind Book    = "book"
showDocClassKind Report  = "report"
showDocClassKind Letter  = "letter"
showDocClassKind (OtherDocumentClassKind x) = x

preambOfDocClass :: DocumentClass -> PreambleItm
preambOfDocClass (DocumentClass kind mpaper msize args) =
  PreambleCmdArgs "documentclass" $
    args ++ [Optionals (maybeToList (LatexLength <$> msize) ++
                        maybeToList (RawTex . showPaper <$> mpaper))
            ,Mandatory . RawTex $ showDocClassKind kind
            ]

ppDocument :: Document -> ShowS
ppDocument (Document docClass preamb doc) =
  ppPreamble (preambOfDocClass docClass <> preamb) $$
  ppEnv "document" [] (ppParMode doc)

usedPackages :: PreambleItm -> Set PackageName
usedPackages x = Set.fromList [ pkg | Usepackage pkg _ <- universe x ]

neededPackages :: Document -> Set PackageName
neededPackages x = Set.fromList [ pkg | pkg@(PkgName _) <- universeBi x ]

showsLaTeX :: LatexM Document -> Either String ShowS
showsLaTeX mdoc = do
  doc <- runLatexM mdoc
  let preamb      = documentPreamble doc
      usedPkgs    = usedPackages preamb
      neededPkgs  = neededPackages doc
      missingPkgs = Set.toList $ neededPkgs `Set.difference` usedPkgs
      preamb'     = preamb <> foldMap (`Usepackage` []) missingPkgs
  return . ppDocument $ doc { documentPreamble = preamb' }

showLaTeX :: LatexM Document -> Either String String
showLaTeX = fmap ($"") . showsLaTeX
