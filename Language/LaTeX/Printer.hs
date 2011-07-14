module Language.LaTeX.Printer where

import Data.Monoid
import Data.List (intersperse)
import Data.Ratio (numerator, denominator)
import Data.Generics.Uniplate.Data (universe, universeBi)
import Data.Foldable (foldMap)
import Data.Set (Set)
import Data.Char
import qualified Data.Set as Set
import GHC.Float (formatRealFloat, FFFormat(FFFixed))

import Language.LaTeX.Types
import Language.LaTeX.Builder.MonoidUtils

optionals :: [a] -> Arg a
optionals [] = NoArg
optionals xs = Optional xs

text :: String -> ShowS
text = showString

between :: String -> String -> ShowS -> ShowS
between opening closing x = text opening ⊕ x ⊕ text closing


braces, brackets, parens :: ShowS -> ShowS
braces   = between "{" "}"
brackets = between "[" "]"
parens   = between "(" ")"


nl, backslash, sp :: ShowS
backslash = text "\\"
nl = text "\n"
sp = text " "

($$) :: ShowS -> ShowS -> ShowS
($$) x y = x ⊕ nl ⊕ y

vcat :: [ShowS] -> ShowS
vcat = mconcat . intersperse (nl ⊕ nl)

ppNamed :: Named ShowS -> ShowS
ppNamed (Named name val) = text name ⊕ text "=" ⊕ val

commas :: [ShowS] -> ShowS
commas = mconcat . intersperse (text ",")

ppArg :: Arg ShowS -> ShowS
ppArg NoArg             = id
ppArg StarArg           = text "*"
ppArg (Optional [])     = error "ppArg: impossible: Optional []"
ppArg (NamedOpts [])    = error "ppArg: impossible: NamedOpts []"
ppArg (Mandatory xs)    = braces   . commas $ xs
ppArg (Optional xs)     = brackets . commas $ xs
ppArg (NamedArgs xs)    = braces   . commas . map ppNamed $ xs
ppArg (NamedOpts xs)    = brackets . commas . map ppNamed $ xs
ppArg (Coordinates x y) = parens (x ⊕ text " " ⊕ y)
ppArg (RawArg x)        = text x
ppArg (PackageDependency _) = id

ppEnv :: String -> [Arg ShowS] -> ShowS -> ShowS
ppEnv envName args contents =
  backslash ⊕ begin ⊕ braces envNameS ⊕ mconcat (map ppArg args)
 $$
  contents
 $$
  backslash ⊕ end ⊕ braces envNameS

  where envNameS = text envName
        begin    = text "begin"
        end      = text "end"

-- these are not wrapped by braces
ppCmdArgs :: String -> [Arg ShowS] -> ShowS
ppCmdArgs cmdName args = backslash ⊕ text cmdName ⊕ mconcat (map ppArg args)

ppDecl :: String -> ShowS -> ShowS
ppDecl declName declArgs = backslash ⊕ text declName ⊕ declArgs ⊕ text " " -- or {}

ppTexDecl :: TexDcl -> ShowS
ppTexDecl (TexDcl declName declArgs) = ppDecl declName (foldMap (ppArg . fmap ppAny) declArgs)

ppMathDecl :: MathDcl -> ShowS
ppMathDecl (MathDcl declName) = ppDecl declName ø

pp :: LatexItm -> ShowS
pp (LatexCmdArgs cmdName args) = ppCmdArgs cmdName $ map (fmap pp) args
pp (LatexCmdAnyArgs cmdName args) = ppCmdArgs cmdName $ map (fmap ppAny) args
pp (TexDecls decls) = foldMap ppTexDecl decls
pp (TexCmdNoArg cmdName) = braces $ ppCmdArgs cmdName []
pp (TexCmdArg cmdName contents)
 = braces (backslash ⊕ text cmdName ⊕ text " " ⊕ pp contents)
pp (Environment envName args contents) = ppEnv envName (map (fmap ppAny) args) $ ppAny contents
pp (RawTex s) = text s
-- One produces $...$ since \(...\) is ``fragile''
pp (LatexCast (MathItm m)) = text "$ " ⊕ ppMath m ⊕ text " $"
pp (LatexCast x) = ppAny x
pp (TexGroup t) = braces $ pp t
pp (LatexConcat contents) = mconcat $ map pp contents
pp (LatexNote key note t) = ppNote key note pp t

ppParMode :: ParItm -> ShowS
ppParMode (ParCast (MathItm m)) = text "\\[ " ⊕ ppMath m ⊕ text " \\]"
ppParMode (ParCast t) = ppAny t
ppParMode (ParCmdArgs cmdName args) = ppCmdArgs cmdName $ map (fmap ppAny) args
ppParMode (RawParMode x) = text x
ppParMode (ParGroup p) = braces $ ppParMode p
ppParMode (ParEnv envName args contents)
  = ppEnv envName (map (fmap ppAny) args) $ ppAny contents
ppParMode (Tabular specs rows) =
  ppEnv "tabular" [Mandatory . (:[]) . mconcat $ map (ppRowSpec . fmap pp) specs] (ppRows pp rows)
ppParMode (ParConcat contents) = vcat $ map ppParMode contents
ppParMode (ParNote key note t) = ppNote key note ppParMode t

ppMath :: MathItm -> ShowS
ppMath (MathDecls decls) = foldMap ppMathDecl decls
ppMath (MathCmdArgs cmdName args) = ppCmdArgs cmdName $ map (fmap ppAny) args
ppMath (RawMath s) = text s
ppMath (MathCast x) = ppAny x
ppMath (MathRat r) | denominator r == 1 = shows (numerator r)
                     | otherwise          = shows (numerator r) ⊕ text " / " ⊕ shows (denominator r)
ppMath (MathArray specs rows) = 
  ppEnv "array" [Mandatory . (:[]) . mconcat $ map (ppRowSpec . fmap ppMath) specs] (ppRows ppMath rows)
ppMath (MathGroup m) = braces $ ppMath m
ppMath (MathConcat ms) = mconcat $ map ppMath ms
ppMath (MathUnOp op m) = text op ⊕ sp ⊕ ppMath m
ppMath (MathBinOp op l r) = parens (ppMath l ⊕ sp ⊕ text op ⊕ sp ⊕ ppMath r)
ppMath (MathNote key note m) = ppNote key note ppMath m

ppAny :: AnyItm -> ShowS
ppAny (PreambleItm x) = ppPreamble x
ppAny (LatexItm    x) = pp x
ppAny (MathItm     x) = ppMath x
ppAny (ParItm      x) = ppParMode x
ppAny (LocSpecs locs) = text . map locSpecChar $ locs
ppAny (Key key)       = text . getKey $ key
ppAny (Length len)    = ppTexLength len
ppAny (Coord (MkCoord x y)) = ppTexLength x ⊕ text " " ⊕ ppTexLength y
ppAny (SaveBin bin) = text $ "\\hlatexSaveBin" ++ (map enc . show $ unsafeGetSaveBin bin)
  where enc i = chr (ord 'a' + digitToInt i) -- hackish but numbers are prohibited
ppAny (PackageName pkg) = text $ getPkgName pkg

ppRowSpec :: RowSpec ShowS -> ShowS
ppRowSpec Rc        = text "c"
ppRowSpec Rl        = text "l"
ppRowSpec Rr        = text "r"
ppRowSpec Rvline    = text "|"
ppRowSpec (Rtext x) = text "@" ⊕ braces x


ppRows :: (a -> ShowS) -> [Row a] -> ShowS
ppRows _ []
  = ø
ppRows ppCell (Cells cells : rows)
  = (mconcat . intersperse (text " & ") . map ppCell $ cells)
 ⊕ (if null rows then ø else backslash ⊕ backslash $$ ppRows ppCell rows)
ppRows ppCell (Hline : rows)
  = backslash ⊕ text "hline " ⊕ ppRows ppCell rows
ppRows ppCell (Cline c1 c2 : rows)
  -- No braces here around the \cline, intentionally
  = ppCmdArgs "cline" [Mandatory . (:[]) . text $ show c1 ++ "-" ++ show c2] ⊕ ppRows ppCell rows

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
    LengthCmdRatArg  cmd r -> braces $ ppCmdArgs cmd [Mandatory . (:[]) $ showr r]
    LengthScaledBy _ (LengthScaledBy _ _) ->
      error "broken invariant: nested LengthScaledBy"
    LengthScaledBy r l     -> showr r ⊕ ppTexLength l
    LengthCst munit r      -> showr r ⊕ foldMap (text . unitName) munit
  where showr r | denominator r == 1 = shows $ numerator r
                | otherwise          = text $ formatRealFloat FFFixed (Just 2) (fromRational r :: Double)

ppPreamble :: PreambleItm -> ShowS
ppPreamble (PreambleCmdArgs cmdName args) = ppCmdArgs cmdName $ map (fmap ppAny) args
ppPreamble (PreambleEnv envName args contents) = ppEnv envName (map (fmap ppAny) args) (ppAny contents)
ppPreamble (PreambleCast x) = ppAny x
ppPreamble (PreambleConcat ps) = vcat $ map ppPreamble ps
ppPreamble (Usepackage pkg opts)
  = ppCmdArgs "usepackage" [optionals (map ppAny opts), Mandatory [text $ getPkgName pkg]]
ppPreamble (RawPreamble raw) = text raw
ppPreamble (PreambleNote key note p) = ppNote key note ppPreamble p

ppNote :: Key -> Note -> (a -> ShowS) -> a -> ShowS
ppNote (MkKey key) note ppElt elt = nl' ⊕ comment (key ⊕ ": " ⊕ showNote note) ⊕ ppElt elt ⊕ nl'
  where nl' = text "%\n"
        showNote (TextNote s) = s
        showNote (IntNote  i) = show i
        showNote (LocNote  loc) = showLoc loc
        comment = mconcat . map (text . ('%':) . (⊕ "\n") . stripRight) . lines
        stripRight = reverse . dropWhile isSpace . reverse

showLoc :: Loc -> String
showLoc (Loc fp line char) = unwords [fp, ":", show line, ":", show char]

showDocClassKind :: DocumentClassKind -> String
showDocClassKind Article = "article"
showDocClassKind Book    = "book"
showDocClassKind Report  = "report"
showDocClassKind Letter  = "letter"
showDocClassKind (OtherDocumentClassKind x) = x

preambOfDocClass :: DocumentClss -> PreambleItm
preambOfDocClass (DocClass kind opts) =
  PreambleCmdArgs "documentclass"
    [optionals opts, Mandatory . (:[]) . LatexItm . RawTex $ showDocClassKind kind]

ppDocument :: Document -> ShowS
ppDocument (Document docClass preamb doc) =
  ppPreamble (preambOfDocClass docClass ⊕ preamb) $$
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
      preamb'     = preamb ⊕ foldMap (`Usepackage` []) missingPkgs
  return . ppDocument $ doc { documentPreamble = preamb' }

showLaTeX :: LatexM Document -> Either String String
showLaTeX = fmap ($"") . showsLaTeX
