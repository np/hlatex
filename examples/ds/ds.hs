{-# LANGUAGE QuasiQuotes, OverloadedStrings, RankNTypes #-}
{-# OPTIONS_GHC -F -pgmF frquotes #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

import Control.Monad.Writer
import Data.List
import Language.LaTeX
import Numeric
import qualified Language.LaTeX.Builder as B
import qualified Language.LaTeX.Builder.Internal as BI
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

align :: (Rational, Pos) -> LatexItem -> LatexItem
align (k, pos) = B.makebox (L.ex (k * 1.22)) pos
check :: (Show a, Eq a) => a -> a -> a
check ref my | my == ref = my
             | otherwise = error $ "check, expecting " ++ show ref ++ " but found " ++ show my
fi = fromInteger
fr = fromRational
rat = BI.rawMath . --
      reverse . dropWhile (=='0') . reverse . -- remove extra 0s
      ($"") . showFFloat (Just 3) . (`asTypeOf`(0::Double)) . fr
includegraphics o f = put $ G.includegraphics o f
minigraphic len f =
  minipage len $ includegraphics (\o -> o{ G.width = Just len }) f
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
prc x = x ⊕ [texm|\%|]
prime x = x ⊕ [texm|'|]
(/′) = M.frac
binop op x y = x ⊕ op ⊕ y
infixr 1 ≡
(≡) = binop [texm|=|]
infixr 1 ≈
(≈) = binop [texm|≈|]
infixl 4 ≥
(≥) = binop [texm|≥|]
infixl 4 ≤
(≤) = binop [texm|≤|]
infixl 6 +′
(+′) = binop [texm|+|]
infixl 6 -′
(-′) = binop [texm|-|]
infixl 7 ×
(×) = binop [texm|×|]
parens x = [texm|(|] ⊕ x ⊕ [texm|)|]
(·) x y = x ⊕ parens y

tabval :: Rational -> MathItem -> [(Integer,String)] -> ParItemW
tabval w f vals =
  let var = [texm|x|] in
  put . B.tabular (B.vline : concat (replicate (1 + length vals) [B.c,B.vline])) $
   [B.hline
   ,B.cells $ m var : map (align (w, B.centered) . M.mstring . show . fst) vals
   ,B.hline
   ,B.cells $ m (f·var) : map (M.mstring . snd) vals
   ,B.hline]

tabval2 :: Rational -> MathItem -> [Integer] -> ParItemW
tabval2 w f range = tabval w f (zip range (repeat ø))

tabval3 :: Rational -> MathItem -> [Integer] -> (Integer -> Integer) -> ParItemW
tabval3 w f range vf = tabval w f (zip range (map (show . vf) range))

data LH = Low | High deriving (Eq)

tabvar :: Int -> MathItem -> [(Integer,LH,Integer)] -> ParItemW
tabvar w f tab =
  put . B.tabular (B.c : B.vline : replicate len B.c) $
   [B.cells $ m var : map (M.mstring . show) range
   ,B.hline
   ,B.cells . map M.mstring $ "":high
   ,B.cells $ m (f·var) : replicate len (M.mstring blank)
   ,B.cells . map M.mstring $ "":low
   ]
  where len = length range
        var = [texm|x|]
        range = [ x | (x,_,_) <- tab ]
        blank = replicate w ' '
        high  = [ if lh == High then show x else blank | (_,lh,x) <- tab ]
        low   = [ if lh == Low  then show x else blank | (_,lh,x) <- tab ]

tabvar3 :: Int -> MathItem -> [(Integer,LH)] -> (Integer -> Integer) -> ParItemW
tabvar3 w f tab vf = tabvar w f (map g tab)
  where g (x, lh) = (x, lh, vf x)

type BOOL = forall a. a -> a -> a

data Config = Config { sujet :: BOOL, correc :: BOOL }

body :: Config -> ParItem
body Config{ sujet = s, correc = c } = execWriter $ do
  let enoncé x = c (const ø) id !$? x
      correction x = c id (const ø) !$? x

  enoncé $
    p [tex|
      |\pagestyle{fancy}
      |\lhead{Prénom et nom :}
      |\rhead{TST2B}
      |\cfoot{}
      |\setcounter{exo}{0}
      |\begin{center}
      |  {\Large \textbf{Contrôle du 17 novembre 2011}}
      |\end{center}
      |\textbf{Indiquer le détail de tous les calculs.}\\
      |]

  correction $
    p [tex|
      |\pagestyle{fancy}
      |\lhead{}
      |\rhead{TST2B}
      |\cfoot{}
      |\setcounter{exo}{0}
      |\begin{center}
      |  {\Large \textbf{Correction du contrôle du 17 novembre 2011}}
      |\end{center}
      |]

  exercice $ do
    let
      n = M.n
      u x = M.u ⊕ M.sub x
      _R = M._R

    enoncé $ do
      p«On considère la suite arithmétique ({m$u n}) telle que
        {m$u 1 ≡ 12} et {m$ s (u 5 ≡ 48) (u 6 ≡ 47)}.
        Quelle est la raison de cette suite ? Justifier.»

    correction $ do
      p«{m$u n} est une suite arithmétique donc {m$ u n ≡ u 1 +′ (n - 1) × _R}.»
      -- TODO could be more compact
      p«Donc {m$ s (u 5 ≡ u 1 +′ (5 - 1) × _R) (u 6 ≡ u 1 +′ (6 - 1) × _R)}.»
      p«Donc {m$ s (48 ≡ 12 +′ 4 × _R) (47 ≡ 12 +′ 5 × _R)}.»
      p«Donc {m$ s (48 -′ 12 ≡ 4 × _R) (47 -′ 12 ≡ 5 × _R)}.»
      p«Donc {m$ s (36 ≡ 4 × _R) (35 ≡ 5 × _R)}.»
      p«Donc {m$ s (36 /′ 4 ≡ _R) (35 /′ 5 ≡ _R)}.»
      p«Donc {m$ _R ≡ s 9 7}.»

  exercice $ do
    let
      v0 :: Num a => a
      v0 = s 5 7
      _R = M._R
      v x = M.v ⊕ M.sub x
      n = M.n
      obs = s 6 4
      _X = M._X

    enoncé $ do
      p«On  injecte  à   un  malade  par  une   intraveineuse  une  dose
      de {m$v0} cm³ d’un  produit donné.  On fait  un relevé  toutes les
      heures de  la quantité,  exprimée en  cm³, de  ce produit  dans le
      sang,  qui  diminue  du  fait de  son  élimination  naturelle  par
      l’organisme. On note {m$v n} le volume de produit, exprimé en cm³,
      dans le sang du malade {m$n}  heures après l’injection. On a ainsi
      {m$v 0  ≡ v0}.  L’observation permet  de conclure  que {m$obs}% du
      produit  est  éliminé toutes  les  heures  par rapport  au  relevé
      précédent.»

      enumerate $ do
        item «Calculer les termes {m$v 1}, {m$v 2} et {m$v 3} (les valeurs
              seront arrondies au millième).»
        item «Écrire {m$v (su n)} en fonction de {m$v n}.»
        item «Quelle  est la nature de  la suite ({m$v n})  ? Préciser son
              terme initial et sa raison.»
        item «En déduire l’écriture de {m$v n} en fonction de {m$n}.»
        item «Au bout  de combien d’heures, reste-t-il moins  de la moitié
              du volume de départ dans le sang du patient ?»

    correction $ do
      p« »
      enumerate $ do
        let r   = 1 - obs / 100
            _Rv = rat r
            vv :: Integer -> Rational
            vv x = v0 * (r ^^ x)
            v1   = rat $ vv 1
            v2   = rat $ vv 2
            v3   = rat $ vv 3
            sol  = fi $ check (s 12 17) (head . filter ((<= (v0/2)) . vv) $ [0..])
        itemW $ do
          p«Il y a une diminution de {m$obs}%.»
          p«Le coefficient multiplicateur
            correspondant est de {m$ 1 -′ prc obs ≡ 1 -′ obs/′100 ≡ _Rv}.»
          p.m $ v 1 ≡ v0 × _Rv ≡ v1
          p.m $ v 2 ≡ v1 × _Rv ≡ v2
          p.m $ v 3 ≡ v2 × _Rv ≡ v3
        item «{m$v (su n) ≡ v n × _Rv}.»
        item «La suite est géométrique de terme initial {m$v0} et de raison {m$_Rv}.»
        item «{m$v n ≡ v 0 × _R ^′ n}. Donc {m$v n ≡ v0 × _Rv ^′ n}.»
        item «On entre dans la calculette la formule {m$ v0 × _Rv ^′ _X}. On fait
              afficher un tableau de valeurs. On cherche la première valeur
              inférieure à {m$ v0 /′ 2 ≡ rat (v0 / 2)}. La première valeur
              est {m$ v sol ≈ rat (vv sol)}.
              C'est donc au bout  de {m$ sol} heures qu'il reste moins  de la moitié
              du volume de départ dans le sang du patient.»

  exercice $ do
    let
      _F = M._F
      _F' = prime _F
      _G = M._G
      _G' = prime M._G
      x = M.x

      domF = s _ℝ (1 <..< (+∞))
      defF = _F·x ≡ s [texm|4x³ - 5x + 3|] [texm|-7x² + 6 + \sqrt{x}|]
      defF' = _F'·x ≡ s [texm|4 × 3x² - 5 × 1 + 0|] [texm|-7 × 2x + 0 + \frac{1}{2\sqrt{x}}|]
      defF'2 = _F'·x ≡ s [texm|12x² - 5|] [texm|-14x + \frac{1}{2\sqrt{x}}|]
      domG = 0 >..< (+∞)
      defG = _G·x ≡ s [texm|3 \sqrt{x} + \frac{1}{x}|] [texm|2 x³ + \frac{5}{x}|]
      defG' = _G'·x ≡ s [texm|3 × \frac{1}{2\sqrt{x}} + \frac{ -1}{x²}|] [texm|2 × 3x² + \frac{ -5}{x²}|]
      defG'2 = _G'·x ≡ s [texm|\frac{3}{2\sqrt{x}} - \frac{1}{x²}|] [texm|6x² - \frac{5}{x²}|]

    enoncé $ do
      p «Calculer la dérivée de chaque fonction suivante.»
      enumerate $ do
        item «la fonction {m$_F} définie sur {m$domF} par {m$defF}»
        item «la fonction {m$_G} définie sur {m$domG} par {m$defG}»
    correction $ do
      p« »
      enumerate $ do
        itemW $ do
          p . m $ defF
          p «La dérivée est définie par {m$defF'}. Donc {m$defF'2}.»
        itemW $ do
          p . m $ defG
          p «La dérivée est définie par {m$defG'}. Donc {m$defG'2}.»

  exercice $ do
    let
      vf x = s (-2 * x2 + 12 * x + 2) (3 * x2 + 6 * x - 5)
        where x2 = x * x
    let
      f  = M.f
      f' = prime f
      x  = M.x
      vdomf = s [-1 .. 5] [-3 .. 3]
      domf = s (-1 <..> 5) (-3 <..> 3)
      deff = f·x ≡ s [texm|-2x² + 12x + 2|] [texm|3x² + 6x - 5|]
      deff' = f'·x ≡ s [texm|-2 × 2x + 12 × 1 + 0|] [texm|3 × 2x + 6 × 1 - 0|]
      maxf = s 20 40
      f'body = s [texm|-4x + 12|] [texm|6x + 6|]
      deff'2 = f'·x ≡ f'body

    enoncé $ do
      p «La fonction {m$f} est définie sur l'intervalle {m$domf} par {m$deff}.»
      enumerate $ do
        item «Calculer la dérivée de {m$f}.»
        item «Etudier le signe de {m$f'}.»
        item «En déduire les variations de la fonction {m$f} et dresser son
              tableau de variations.»
        itemW $ do
          p «Compléter le tableau de valeurs suivant:»
          tabval2 8 "f" vdomf
        item «Tracer dans un repère la courbe représentative de la fonction {m$f}.»
        item «Quel est le maximum de la fonction {m$f} ?»

    correction $ do
      p« »
      enumerate $ do
        itemW $ do
          p . m $ deff
          p «La dérivée est définie par {m$deff'}. Donc {m$deff'2}.»
        let f'pos = s (-1 <..> 3) (-1 <..> 3)
            f'neg = s (3 <..> 5) (-3 <..> -1)
        itemW $ do
          p «Étude du signe de {m$f'}:»
          p «{m$f'·x} est positif si {m$f'body} est positif.»
          p «Si {m$f'body ≥ 0}.»
          p «Si {m$s [texm|-4x ≥ -12|] [texm|6x ≥ -6|]}.»
          p «Si {m$s (x ≤ (-12) /′ (-4)) (x ≥ (-6) /′ 6)}.»
          p «Si {m$s (x ≤ 3) (x ≥ -1)}.»
          p «Conclusion :»
          let inf = «inférieur»
              sup = «supérieur»
          p «{m$f'·x} est positif quand {m$x} est {s inf sup} à {m$s 3 (-1)},
             c'est-à-dire quand {m$x} appartient à l'intervalle {m$f'pos}.»
          p «{m$f'·x} est négatif quand {m$x} est {s sup inf} à {m$s 3 (-1)},
             c'est-à-dire quand {m$x} appartient à l'intervalle {m$f'neg}.»
        itemW $ do
          p «{m f} est croissante sur l'intervalle {m$f'pos} car {m f'} est
             positive. {m f} est décroissante sur l'intervalle {m$f'neg}
             car {m f'} est négative.»
          tabvar3 8 "f" (s [(-1,Low), (3,High),(5,Low)]
                           [(-3,High), (-1,Low), (3,High)]) vf
        itemW $ tabval3 8 "f" vdomf vf
        let
          fig = s "../courbe1.pdf"
                  "../courbe2.pdf"
        itemW $ do
          p « »
          includegraphics id fig
        item «Le maximum de la fonction {m$f} est {m$maxf}.»

  let
    g  = M.g
    g' = prime g
    domg = s ((-∞) >..< 0) (0 >..< (+∞))
    _C = M.mathcal "C"
    _T = M._T
    fig = s "../fig1B.pdf" -- 1 / x²
            "../fig2B.pdf" -- 2 / x²
    abstan = s (-1) 1

  exercice $ do
    enoncé $ do
      p«Soit {m$g}  la  fonction  définie sur {m$domg}  dont  la  courbe
      représentative {m$ _C}  est donnée  ci-dessous. La  droite {m$ _T}
      est la tangente à la courbe au point d'abscisse {m$abstan}.»

      pW $ do
        minigraphic (L.cm 9.5) fig

        minipage (L.cm 10) $ do
          enumerate $ do
            item «Donner la définition de nombre dérivé.»
            item «Déterminer {m$ g'·abstan}. Justifier.»
            item «Déterminer l'équation de {m$ _T}.»

    correction $ do
      p « »
      let y = M.y
      enumerate $ do
        item «Le  nombre dérivé en {ma|a|} est  le coefficient directeur
              de la tangente à la courbe au point d'abscisse {ma|a|}.»
        item «{m$ _T} est la tangente à la courbe au point d'abscisse {m$ s (-1) 1}.
              Son coefficient directeur est égal à {m$ s (2 /′ 1 ≡ 2) (-4 /′ 1 ≡ -4)}.
              Donc {m$ g'·abstan ≡ s 2 (-4)}.»
        item «L'équation d'une droite est de la forme {ma|y = mx + p|}.
              Le coefficient directeur {ma|m|} est égal à {m$ s 2 (-4)}.
              L'ordonnée à l'origine est l'ordonnée du point d'intersection de la tangente et
              de l'axe des ordonnées. Elle est égale à {m$ s 3 6}.
              Donc l'équation de {m$ _T} est {m$ y ≡ s [texm|2x + 3|] [texm|-4x + 6|]}.»

tt, ff :: BOOL
tt x _ = x
ff _ x = x

doc = B.document docclass preamble (mconcat . intersperse B.newpage . map body $ configs)
  -- where configs = [Config s c | s <- [tt,ff], c <- [ff,tt]]
  where configs = [Config tt ff, Config ff ff, Config tt tt, Config ff tt]

main = quickView myViewOpts{basedir="out",showoutput=False,pdfviewer=":"} "ds" doc
