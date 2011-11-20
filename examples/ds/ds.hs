{-# LANGUAGE QuasiQuotes, OverloadedStrings, RankNTypes #-}
{-# OPTIONS_GHC -F -pgmF frquotes #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

import Control.Monad.Writer
import Data.List
import Language.LaTeX
import qualified Language.LaTeX.Builder as B
import qualified Language.LaTeX.Builder.Math as M
import qualified Language.LaTeX.Length as L
import qualified Language.LaTeX.Builder.Graphics as G
import Language.LaTeX.Builder.QQ

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

data LH = Low | High

tabvar :: Int -> String -> [(Integer,LH,Integer)] -> ParItemW
tabvar w f tab =
  let range = [ x | (x,_,_) <- tab ]
      high  = [ x | (_,lh,x) <- tab ]
      low   = [ x | (_,lh,x) <- tab ]
  put . B.tabular (B.vline : concat (replicate (1 + len) [B.c,B.vline])) $
   [B.hline
   ,B.cells . map M.mstring $ ("x":map show range)
   ,B.hline
   ,B.cells . map M.mstring $ ("":replicate len (replicate w ' '))
   ,B.hline
   ,B.cells . map M.mstring $ ((f ++ "(x)"):replicate len (replicate w ' '))
   ,B.hline
   ,B.cells . map M.mstring $ ("":replicate len (replicate w ' '))
   ,B.hline]
  where len = length range

type BOOL = forall a. a -> a -> a

data Config = Config { sujet :: BOOL, correc :: BOOL }

body :: Config -> ParItem
body Config{ sujet = s, correc = c } = execWriter $ do
  let enoncé = c (const ø) id
      correction = c id (const ø)

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

  exercice $ do
    let
      m = B.math
      n = M.n
      u x = M.u ⊕ M.sub x
      su x = x ⊕ [texm|+ 1|] -- x + 1
      _R = M._R

      ueq = s (u 5 === 48) (u 6 === 47)

    enoncé $ do
      p«On considère la suite arithmétique ({m$u n}) telle que
        {m$u 1 === 12} et {m$ueq}. Quelle est la raison de cette suite ?
        Justifier.»

    correction $ do
      p«{m$u n} est une suite arithmétique donc {u n === u 1 + (n - 1) * _R}.»
      p«Donc {u 5 === u 1 + (5 - 1) * _R}.»
      p«Donc {48 === 12 + 4 * _R}.»
      p«Donc {48 - 12 === 4 * _R}.»
      p«Donc {36 === 4 * _R}.»
      p«Donc {36 / 4 === _R}.»
      p«Donc {_R === 9}.»

  exercice $ do
    let
      v0 :: MathItem
      v0 = s 5 7
      v x = M.v ⊕ M.sub x
      obs = s 6 4
      _X = M._X

    enoncé $ do
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

    correction $ do
      enumerate $ do
        item $ do
          p«Il y a une diminution de {m$obs}%. Le coefficient multiplicateur
            correspondant est de {m$ 1 - obs === 1 - (6/100) === 0.94}.»
          p«{m$v 1 === v0 * 0.94 === 4.7}.»
          p«{m$v 2 === 4.7 * 0.94 === 4.418}.»
          p«{m$v 3 === 4.418 * 0.94 === 4.153}.»
        item «{m$v (suc n) === v n * 0.94}.»
        item «La suite est géométrique de terme initial {v0} et de raison {m$0.94}.»
        item «{m$v n === v 0 * _R ^ n}. Donc {m$v n === v0 * 0.94 ^ n}.»
        item «On entre dans la calculette la formule {v0 * 0.94 ^ _X}. On fait
              afficher un tableau de valeurs. On cherche la première valeur
              inférieure à {m$ 5 / 2 === 2.5}. La première valeur est {v 12 ≈ 2.38}.
              C'est donc au bout  de {m$12} heures qu'il reste moins  de la moitié
              du volume de départ dans le sang du patient.»

  exercice $ do
    let
      _F = M._F
      _G = M._G

      domF = s _ℝ (1 <..< (+∞))
      defF = [texm|F(x)|] === s [texm|4x³ - 5x + 3|] [texm|-7x² + 6 + \sqrt{x}|]
      defF' = [texm|F'(x)|] === s [texm|4 \times 3x² - 5 \times 1 + 0|] [texm|-7 \times 2x + 0 + \frac{1}{2\sqrt{x}}|]
      defF'2 = [texm|F'(x)|] === s [texm|12x² - 5|] [texm|-14x + \frac{1}{2\sqrt{x}}|]
      domG = 0 >..< (+∞)
      defG = [texm|G(x)|] === s [texm|3 \sqrt{x} + \frac{1}{x}|] [texm|2 x³ + \frac{5}{x}|]
      defG' = [texm|G'(x)|] === s [texm|3 \times \frac{1}{2\sqrt{x}} + \frac{ -1}{x²}|] [texm|2 \times 3x² + \frac{ -5}{x²}|]
      defG'2 = [texm|G'(x)|] === s [texm|\frac{3}{2\sqrt{x}} - \frac{1}{x²}|] [texm|6x² - \frac{5}{x²}|]

    enoncé $ do
      p «Calculer la dérivée de chaque fonction suivante.»
      enumerate $ do
        item «la fonction {m$_F} définie sur {m$domF} par {m$defF}»
        item «la fonction {m$_G} définie sur {m$domG} par {m$defG}»
    correction $ do
      enumerate $ do
        itemW $ do
          p . m $ defF
          p «La dérivée est définie par {m$defF'}. Donc {m$defF'2}.»
        itemW $ do
          p . m $ defG
          p «La dérivée est définie par {m$defG'}. Donc {m$defG'2}.»

  exercice $ do
    let
      f  = M.f
      f' = [texm|f'|]
      dom_f = s [-1 .. 5] [-3 .. 3]
      domf = s (-1 <..> 5) (-3 <..> 3)
      deff = [texm|f(x)|] === s [texm|-2x² + 12x + 2|] [texm|3x² + 6x - 5|]
      deff' = [texm|f'(x)|] === s [texm|-2 \times 2x + 12 \times 1 + 0|] [texm|3 \times 2x + 6 \times 1 - 0|]
      f'body = [texm|-4x + 12|]
      deff'2 = [texm|f'(x)|] === s f'body [texm|6x + 6|]

    enoncé $ do
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

    correction $ do
      enumerate $ do
        itemW $ do
          p . m $ deff
          p «La dérivée est définie par {m$deff'}. Donc {m$deff'2}.»
        itemW $ do
          p «Étude du signe de {m$f'}:»
          p «{ma|f'(x)|} est positif si {m$f'body} est positif.»
          p «Si {m$f'body > 0}.»
          p «Si {ma|-4x > -12|}.»
          p «Si {ma|x < \frac{-12}{-4}|}.»
          p «Si {ma|x < 3|}.»
          p «Conclusion : {ma|f'(x)|} est positif quand {m$x} est inférieur à {m$3}, c'est-à-dire quand {m$x} appartient
            »
          p «{ma|f'(x)|} est négatif quand {m$x} est supérieur à {m$3}.»
        itemW $ do
          p «{ma|f|} est croissante sur l'intervalle {m$ (-1) <..> 3} et décroissante sur
             l'intervalle {m$ 3 <..> 5}.»
          

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
        put $ G.includegraphics (\o -> o{ G.width = Just (L.cm 9.5) }) fig

      minipage (L.cm 10) $ do
        enumerate $ do
          item «Donner la définition de nombre dérivé.»
          item «Déterminer {s [ma|g'(-1)|] [ma|g'(1)|]}. Justifier.»
          item «Déterminer l'équation de {m$ _T}.»

tt, ff :: BOOL
tt x _ = x
ff _ x = x

doc = B.document docclass preamble (mconcat . intersperse B.newpage . map body $ configs)
  where configs = [Config tt tt]

main = quickView myViewOpts{basedir="out",showoutput=False,pdfviewer=":"} "ds" doc
