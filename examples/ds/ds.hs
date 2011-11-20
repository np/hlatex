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

m = B.math
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
(^′) x y = x ⊕ M.sup y
su x = x ⊕ [texm|+ 1|] -- x + 1
(/′) = M.frac
binop op x y = x ⊕ op ⊕ y
infixr 1 ≡
(≡) = binop [texm|=|]
infixr 1 ≈
(≈) = binop [texm|≈|]
infixl 4 >′
(>′) = binop [texm|>|]
infixl 6 +′
(+′) = binop [texm|+|]
infixl 6 -′
(-′) = binop [texm|-|]
infixl 7 *′
(*′) = binop [texm|\times{}|]

tabval :: String -> [(Integer,String)] -> ParItemW
tabval f vals =
  put . B.tabular (B.vline : concat (replicate (1 + length vals) [B.c,B.vline])) $
   [B.hline
   ,B.cells . map M.mstring $ "x":map (show . fst) vals
   ,B.hline
   ,B.cells . map M.mstring $ (f ++ "(x)"):map snd vals
   ,B.hline]

tabval2 :: Int -> String -> [Integer] -> ParItemW
tabval2 w f range = tabval f (zip range (repeat blank))
  where blank = replicate w ' '

tabval3 :: String -> [Integer] -> (Integer -> Integer) -> ParItemW
tabval3 f range vf = tabval f (zip range (map (show . vf) range))

data LH = Low | High deriving (Eq)

tabvar :: Int -> String -> [(Integer,LH,Integer)] -> ParItemW
tabvar w f tab =
  put . B.tabular (B.c : B.vline : replicate len B.c) $
   [B.cells . map M.mstring $ "x":map show range
   ,B.hline
   ,B.cells . map M.mstring $ "":high
   ,B.cells . map M.mstring $ (f ++ "(x)"):replicate len blank
   ,B.cells . map M.mstring $ "":low
   ]
  where len = length range
        range = [ x | (x,_,_) <- tab ]
        blank = replicate w ' '
        high  = [ if lh == High then show x else blank | (_,lh,x) <- tab ]
        low   = [ if lh == Low  then show x else blank | (_,lh,x) <- tab ]

type BOOL = forall a. a -> a -> a

data Config = Config { sujet :: BOOL, correc :: BOOL }

body :: Config -> ParItem
body Config{ sujet = s, correc = c } = execWriter $ do
  let enoncé x = c (const ø) id !$? x
      correction x = c id (const ø) !$? x

  p [tex|
    |\pagestyle{fancy}
    |\lhead{Prénom et nom :}
    |\rhead{TST2B}
    |\cfoot{}
    |\setcounter{exo}{0}
    |]

  enoncé $
    p [tex|
      |\begin{center}
    |    {\Large \textbf{Contrôle du 17 novembre 2011}}
      |\end{center}
      |\textbf{Indiquer le détail de tous les calculs.}\\
      |]

  correction $
    p [tex|
      |\begin{center}
      |  {\Large \textbf{Correction du contrôle du 17 novembre 2011}}
      |\end{center}
      |]

  exercice $ do
    let
      n = M.n
      u x = M.u ⊕ M.sub x
      _R = M._R

      ueq = s (u 5 ≡ 48) (u 6 ≡ 47)

    enoncé $ do
      p«On considère la suite arithmétique ({m$u n}) telle que
        {m$u 1 ≡ 12} et {m$ueq}. Quelle est la raison de cette suite ?
        Justifier.»

    correction $ do
      p«{m$u n} est une suite arithmétique donc {m$ u n ≡ u 1 +′ (n - 1) *′ _R}.»
      p«Donc {m$ u 5 ≡ u 1 +′ (5 - 1) *′ _R}.»
      p«Donc {m$ 48 ≡ 12 +′ 4 *′ _R}.»
      p«Donc {m$ 48 -′ 12 ≡ 4 *′ _R}.»
      p«Donc {m$ 36 ≡ 4 *′ _R}.»
      p«Donc {m$ 36 /′ 4 ≡ _R}.»
      p«Donc {m$ _R ≡ 9}.»

  exercice $ do
    let
      v0 :: MathItem
      v0 = s 5 7
      _R = M._R
      v x = M.v ⊕ M.sub x
      n = M.n
      obs = s 6 4
      _X = M._X

    enoncé $ do
      p«On injecte  à un malade par  une intraveineuse une dose  de {m$v0}
        cm³ d’un produit donné. On fait  un relevé toutes les heures de la
        quantité, exprimée en cm³, de ce produit dans le sang, qui diminue
        du fait de son élimination naturelle par l’organisme. On note {m$v
        n} le  volume de produit, exprimé  en cm³, dans le  sang du malade
        {m$n}  heures  après l’injection.  On  a  ainsi  {m$v 0  ≡  v0}.
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
      p« »
      enumerate $ do
        let _Rv = [texm|0.94|]
        itemW $ do
          p«Il y a une diminution de {m$obs}%. Le coefficient multiplicateur
            correspondant est de {m$ 1 -′ obs ≡ 1 -′ 6/′100 ≡ _Rv}.»
          {-
          p«{m$v 1 ≡ v0 *′ _Rv ≡ 4.7}.»
          p«{m$v 2 ≡ 4.7 *′ _Rv ≡ 4.418}.»
          p«{m$v 3 ≡ 4.418 *′ _Rv ≡ 4.153}.»
          -}
          p«{ma|v₁ = 5 \times 0.94 = 4.7|}.»
          p«{ma|v₂ = 4.7 \times 0.94 = 4.418|}.»
          p«{ma|v₃ = 4.418 \times 0.94 = 4.153|}.»
        item «{m$v (su n) ≡ v n *′ _Rv}.»
        item «La suite est géométrique de terme initial {m$v0} et de raison {m$_Rv}.»
        item «{m$v n ≡ v 0 *′ _R ^′ n}. Donc {m$v n ≡ v0 *′ _Rv ^′ n}.»
        item «On entre dans la calculette la formule {m$ v0 *′ _Rv ^′ _X}. On fait
              afficher un tableau de valeurs. On cherche la première valeur
              inférieure à {m$ 5 /′ 2 ≡ [texm|2.5|]}. La première valeur est {m$ v 12 ≈ [texm|2.38|]}.
              C'est donc au bout  de {m$12} heures qu'il reste moins  de la moitié
              du volume de départ dans le sang du patient.»

  exercice $ do
    let
      _F = M._F
      _G = M._G

      domF = s _ℝ (1 <..< (+∞))
      defF = [texm|F(x)|] ≡ s [texm|4x³ - 5x + 3|] [texm|-7x² + 6 + \sqrt{x}|]
      defF' = [texm|F'(x)|] ≡ s [texm|4 \times 3x² - 5 \times 1 + 0|] [texm|-7 \times 2x + 0 + \frac{1}{2\sqrt{x}}|]
      defF'2 = [texm|F'(x)|] ≡ s [texm|12x² - 5|] [texm|-14x + \frac{1}{2\sqrt{x}}|]
      domG = 0 >..< (+∞)
      defG = [texm|G(x)|] ≡ s [texm|3 \sqrt{x} + \frac{1}{x}|] [texm|2 x³ + \frac{5}{x}|]
      defG' = [texm|G'(x)|] ≡ s [texm|3 \times \frac{1}{2\sqrt{x}} + \frac{ -1}{x²}|] [texm|2 \times 3x² + \frac{ -5}{x²}|]
      defG'2 = [texm|G'(x)|] ≡ s [texm|\frac{3}{2\sqrt{x}} - \frac{1}{x²}|] [texm|6x² - \frac{5}{x²}|]

    enoncé $ do
      p «Calculer la dérivée de chaque fonction suivante.»
      enumerate $ do
        item «la fonction {m$_F} définie sur {m$domF} par {m$defF}»
        item «la fonction {m$_G} définie sur {m$domG} par {m$defG}»
    correction $ do
      p« »
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
      vdomf = s [-1 .. 5] [-3 .. 3]
      domf = s (-1 <..> 5) (-3 <..> 3)
      deff = [texm|f(x)|] ≡ s [texm|-2x² + 12x + 2|] [texm|3x² + 6x - 5|]
      vf x = s (-2 * x2 + 12 * x + 2) (3 * x2 + 6 * x - 5)
        where x2 = x * x
      deff' = [texm|f'(x)|] ≡ s [texm|-2 \times 2x + 12 \times 1 + 0|] [texm|3 \times 2x + 6 \times 1 - 0|]
      f'body = [texm|-4x + 12|]
      deff'2 = [texm|f'(x)|] ≡ s f'body [texm|6x + 6|]
    let
      x  = M.x

    enoncé $ do
      p «La fonction {m$f} est définie sur l'intervalle {m$domf} par {m$deff}.»
      enumerate $ do
        item «Calculer la dérivée de {m$f}.»
        item «Etudier le signe de {m$f'}.»
        item «En déduire les variations de la fonction {m$f} et dresser son
              tableau de variations.»
        itemW $ do
          p «Compléter le tableau de valeurs suivant:»
          tabval2 10 "f" vdomf
        item «Tracer dans un repère la courbe représentative de la fonction
              {m$f}.»
        item «Quel est le maximum de la fonction {m$f} ?»

    correction $ do
      p« »
      enumerate $ do
        itemW $ do
          p . m $ deff
          p «La dérivée est définie par {m$deff'}. Donc {m$deff'2}.»
        itemW $ do
          p «Étude du signe de {m$f'}:»
          p «{ma|f'(x)|} est positif si {m$f'body} est positif.»
          p «Si {m$f'body >′ 0}.»
          p «Si {ma|-4x > -12|}.»
          p «Si {ma|x < \frac{-12}{-4}|}.»
          p «Si {ma|x < 3|}.»
          p «Conclusion : {ma|f'(x)|} est positif quand {m$x} est inférieur à {m$3},
             c'est-à-dire quand {m$x} appartient à l'intervalle {m$ (-1) <..> 3}.»
          p «{ma|f'(x)|} est négatif quand {m$x} est supérieur à {m$3},
             c'est-à-dire quand {m$x} appartient à l'intervalle {m$ 3 <..> 5}.»
        itemW $ do
          p «{ma|f|} est croissante sur l'intervalle {m$ (-1) <..> 3} car {ma|f'|} est
             positive et décroissante sur l'intervalle {m$ 3 <..> 5} car {ma|f'|} est négative.»
          tabvar 10 "f" [(-1, Low, -12),(3,High,20),(5,Low,12)]
        itemW $ tabval3 "f" vdomf vf
        item «COURBE»
        item «Le maximum de la fonction {m$f} est {m$20}.»

  let
    g  = M.g
    domg = s ((-∞) >..< 0) (0 >..< (+∞))
    _C = M.mathcal "C"
    _T = M._T
    fig = s "../fig1B.pdf" -- 1 / x²
            "../fig2B.pdf" -- 2 / x²
    abstan = s (-1) 1

  exercice $ do
    enoncé $ do
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

    correction $ do
      enumerate $ do
        item «Le nombre dérivé, c'est le coefficient directeur de la tangente à la courbe.»
        item «{s [ma|g'(-1) = 2|] [ma|g'(1) = -4|]}»
        item «{s [ma|y = 2x + 3|] [ma|y = -4x + 6|]}»

tt, ff :: BOOL
tt x _ = x
ff _ x = x

doc = B.document docclass preamble (mconcat . intersperse B.newpage . map body $ configs)
  -- where configs = [Config s c | s <- [tt,ff], c <- [ff,tt]]
  where configs = [Config tt ff, Config ff ff, Config tt tt, Config ff tt]

main = quickView myViewOpts{basedir="out",showoutput=False,pdfviewer=":"} "ds" doc
