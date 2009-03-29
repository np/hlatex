{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-missing-signatures #-}
import Language.LaTeX
import qualified Language.LaTeX.Builder as B
import qualified Language.LaTeX.Builder.Rotating as R
import qualified Language.LaTeX.Builder.Color as C
import qualified Language.LaTeX.Builder.Math as M
import Data.Char
import Data.List (intersperse, intercalate)
import Control.Arrow
import Data.String

main = quickView testViewOpts "knowledges-tabular" root

root = B.root preamb $ B.document (B.section "Youhou" <> B.para B.noindent <> table)

preamb = B.documentclass (Just (B.pt 11)) (Just B.a4paper) B.article
      <> B.usepackage [B.optional "latin1"] (B.pkgName "inputenc")

data Grade = A | B | C | Z

gradeToLatex g =
  case g of
    A -> f C.green
    B -> f C.orange
    C -> f C.red
    Z -> f C.white
  where f c = C.colorbox c $
              B.raisebox (Em (-0.1)) $
              B.makebox (Em 2) Stretch $
              C.textcolor c $
              B.hrulefill <> "X" <> B.hrulefill

intersperse' :: a -> [a] -> [a]
intersperse' x xs = x : intersperse x xs ++ [x]

table :: ParItem
table =
  B.tabular tabspec $
    intersperse' B.hline $
      B.cells (("Contrôle n"<>B.textdegree<>"1") : map (R.turn 90 . B.decl B.small) header) : tablebodyTex
  where tabspec = concat (intersperse' [B.vline]
                  ([B.l] : intercalate [[B.rtext (B.hspace (Em 1))]]
                                     (let c = [B.rtext "", B.c, B.rtext ""] in
                                     [replicate 4 c, replicate 4 c, replicate 2 c])))

header :: [LatexItem]
header =
    ["Déterminer le caractère étudié"
    ,"Calculer une fréquence"
    ,"Calculer une moyenne à partir de fréquences"
    ,"Calculer une médiane"
    ,"Calculer une moyenne"
    ,"Calculer une moyenne à partir de sous-groupes"
    ,"Calculer une étendue"
    ,"Utiliser la linéarité de la moyenne"
    ,"Démontrer en utilisant le théorème de Thalès"
    ,"Démontrer en utilisant le théorème de Pythagore"<>B.nbsp]

tablebodyTex :: [Row LatexItem]
tablebodyTex =
  map (B.cells . uncurry (:) . (fromString *** map gradeToLatex)) tablebody

tablebody :: [(String, [Grade])]
tablebody =
  [("Mariam",      [A, C, A, C, A, A, A, B, A, A])
  ,("Soraya",      [A, A, A, B, A, C, A, A, C, C])
  ,("Lynda",       [A, C, C, C, A, C, A, A, C, C])
  ,("Safa",        [A, C, C, C, A, A, C, C, C, C])
  ,("Giovanetti",  [A, A, C, C, A, A, A, B, A, A])
  ,("Hanen",       [A, A, A, A, A, A, A, C, B, C])
  ,("Vincent",     [A, A, A, C, A, A, A, C, B, B])
  ,("Elodie",      [A, A, A, A, A, C, A, C, A, A])
  ,("Mickaël",     [A, C, C, A, A, C, C, C, C, B])
  ,("Channa",      [A, A, A, A, A, A, A, C, C, B])
  ,("Tatiana",     [A, A, A, C, A, C, C, C, B, C])
  ,("Sonia",       [A, A, C, C, A, C, C, C, C, B])
  ,("Diana",       [A, C, C, C, A, A, C, C, C, B])
  ,("Eline",       [A, A, C, C, A, A, A, B, B, A])
  ,("Kemappiriya", [A, A, C, C, A, A, A, C, A, A])
  ,("Yacine",      [A, B, A, A, A, A, C, A, A, A])
  ,("Aimée",       [A, C, C, C, C, C, A, C, C, C])
  ,("Tania",       [A, A, A, A, A, A, A, A, A, C])
  ,("Dottie",      [C, C, C, C, A, C, B, C, C, C])
  ,("Weded",       [C, A, C, A, A, A, B, C, A, B])
  ,("Kimberley",   [C, C, C, C, A, C, B, C, C, C])
  ,("Candice",     [A, A, A, C, A, A, C, C, A, B])
  ,("Moisa",       [A, A, A, C, A, A, B, C, C, A])
  ,("Dany (absent)", replicate 10 Z)
  ]
