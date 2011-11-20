{-# LANGUAGE QuasiQuotes, OverloadedStrings, RankNTypes #-}
{-# OPTIONS_GHC -F -pgmF frquotes #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

import Control.Monad.Writer
import Language.LaTeX
import qualified Language.LaTeX.Builder as B
import qualified Language.LaTeX.Builder.Math as M
import qualified Language.LaTeX.Length as L
import Language.LaTeX.Builder.QQ
import Language.LaTeX.Builder.Graphics

import Kit

-- The document class of your document
docclass = B.article (Just (L.pt 11)) (Just B.a4paper) []

-- Some preliminary declarations
preamble =
  usepackages["graphicx" -- pour inclure des graphiques
             ,"ucs"      -- pour les charactères unicodes
             ,"latexsym" -- pour les symboles
             ,"amsmath"
             ,"amssymb"  -- pour les symboles mathematiques
             ,"geometry" -- pour diminuer les marges
             ,"fancyhdr"
             ,"ntheorem"
          -- ,"eurosym"
          -- ,"setspace"
             ]
  ⊕
  [qp|
  |\usepackage[french]{babel}
  |\usepackage[utf8]{inputenc}
  |\geometry{hmargin=2cm, vmargin=2cm}
  |\theorembodyfont{\rmfamily}
  |\newtheorem{exo}{Exercice}
  |]

interval lc rc low high = mconcat[M.mleft lc, low, [texm|;|], high, M.mright rc]
(<..<) = interval '[' '['
(<..>) = interval '[' ']'
(>..<) = interval ']' '['
(>..>) = interval ']' ']'
infix 3 <..<
infix 3 <..>
infix 3 >..<
infix 3 >..>
(+∞) = [texm|+ ∞|]
(-∞) = [texm|- ∞|]
_ℝ = M.mathbb "R"
(===) x y = x ⊕ [texm|=|] ⊕ y

tabval :: Int -> String -> [Integer] -> ParItemW
tabval w f range =
  put . B.tabular (B.vline : concat (replicate (1 + len) [B.c,B.vline])) $
   [B.hline
   ,B.cells . map M.mstring $ ("x":map show range)
   ,B.hline
   ,B.cells . map M.mstring $ ((f ++ "(x)"):replicate len (replicate w ' '))
   ,B.hline]
  where len = length range

body :: (forall a. a -> a -> a) -> ParItem
body s = execWriter $ do
  p [tex|
    |\pagestyle{fancy}
    |\lhead{Prénom et nom :}
    |\rhead{TST2B}
    |\cfoot{}
    |\begin{center}
    |    {\Large \textbf{Contrôle du 17 novembre 2011}}
    |\end{center}
    |
    |\textbf{Indiquer le détail de tous les calculs.}\\
    |\setcounter{exo}{0}
    |]

  let
    m = B.math
    n = M.n
    u x = M.u ⊕ M.sub x
    su x = x ⊕ [texm|+ 1|] -- x + 1

    ueq = s (u 5 === 48) (u 6 === 47)

  exercice $ do
    p«On considère la suite arithmétique ({m$u n}) telle que
      {m$u 1 === 12} et {m$ueq}. Quelle est la raison de cette suite ?
      Justifier.»

  let
    v0 :: MathItem
    v0 = s 5 7
    v x = M.v ⊕ M.sub x
    obs = s 6 4

  exercice $ do
    p«On injecte  à un malade par  une intraveineuse une dose  de {m$v0}
      cm³ d’un produit donné. On fait  un relevé toutes les heures de la
      quantité, exprimée en cm³, de ce produit dans le sang, qui diminue
      du fait de son élimination naturelle par l’organisme. On note {m$v
      n} le  volume de produit, exprimé  en cm³, dans le  sang du malade
      {m$n}  heures  après l’injection.  On  a  ainsi  {m$v 0  ===  v0}.
      L’observation  permet  de conclure  que  {m$obs}%  du produit  est
      éliminé toutes les heures par rapport au relevé précédent.»

    enumerate $ do
      item «Calculer les termes {m$v 1}, {m$v 2} et {m$v 3} (les valeurs
            seront arrondies au millième).»
      item «Écrire {m$v (su n)} en fonction de {m$v n}.»
      item «Quelle  est la nature de  la suite ({m$v n})  ? Préciser son
            terme initial et sa raison.»
      item «En déduire l’écriture de {m$v n} en fonction de {m$n}.»
      item «Au bout  de combien d’heures, reste-t-il moins  de la moitié
            du volume de départ dans le sang du patient ?»

  let
    _F = M._F
    _G = M._G

    domF = s _ℝ (1 <..< (+∞))
    defF = [texm|F(x)|] === s [texm|4x³ - 5x + 3|] [texm|-7x² + 6 + \sqrt{x}|]
    domG = 0 >..< (+∞)
    defG = [texm|G(x)|] === s [texm|3 \sqrt{x} + \frac{1}{x}|] [texm|2 x³ + \frac{5}{x}|]

  exercice $ do
    p «Calculer la dérivée de chaque fonction suivante.»
    enumerate $ do
      item «la fonction {m$_F} définie sur {m$domF} par {m$defF}»
      item «la fonction {m$_G} définie sur {m$domG} par {m$defG}»

  let
    f  = M.f
    f' = [texm|f'|]
    dom_f = s [-1 .. 5] [-3 .. 3]
    domf = s (-1 <..> 5) (-3 <..> 3)
    deff = [texm|f(x)|] === s [texm|-2x² + 12x + 2|] [texm|3x² + 6x - 5|]

  exercice $ do
    p «La fonction {m$f} est définie sur l'intervalle {m$domf} par {m$deff}.»
    enumerate $ do
      item «Calculer la dérivée de {m$f}.»
      item «Etudier le signe de {m$f'}.»
      item «En déduire les variations de la fonction {m$f} et dresser son
            tableau de variations.»
      itemW $ do
        p «Compléter le tableau de valeurs suivant:»
        tabval 10 "f" dom_f
      item «Tracer dans un repère la courbe représentative de la fonction
            {m$f}.»
      item «Quel est le maximum de la fonction {m$f} ?»

  let
    g  = M.g
    domg = s ((-∞) >..< 0) (0 >..< (+∞))
    _C = M.mathcal "C"
    _T = M._T
    fig = s "../fig1B.pdf" -- 1 / x²
            "../fig2B.pdf" -- 2 / x²
    abstan = s (-1) 1

  exercice $ do
    p«Soit  {m$g}  la  fonction  définie   sur  {m$domg}  dont  la  courbe
      représentative {m$ _C} est  donnée ci-dessous. La droite  {m$ _T} est
      la tangente à la courbe au point d'abscisse {m$abstan}.»

    pW $ do
      minipage (L.cm 9.5) $ do
        put $ includegraphics (\o -> o{ width = Just (L.cm 9.5) }) fig

      minipage (L.cm 10) $ do
        enumerate $ do
          item «Donner la définition de nombre dérivé.»
          item «Déterminer {s [ma|g'(-1)|] [ma|g'(1)|]}. Justifier.»
          item «Déterminer l'équation de {m$ _T}.»

sujet1, sujet2 :: a -> a -> a
sujet1 x _ = x
sujet2 _ x = x

doc = B.document docclass preamble (mconcat[body sujet1, B.newpage, body sujet2])

main = quickView myViewOpts{basedir="out",showoutput=False,pdfviewer=":"} "ds" doc
