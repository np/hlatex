module Language.LaTeX.Builder.Math
  
  -- NOTE: do not forget to update allMathItems, allMathDecls
  (amsmath,
   charToMath, stringToMath, mchar, mstring, mathlift, protect, protector, verb,
   _Delta, _Gamma, _Lambda, _Leftarrow, _Leftrightarrow, _Omega, _Phi, _Pi, _Pr,
   _Rightarrow, _Sigma, _Theta, _Xi, acute, aleph, alpha, approx, array, at,
   backslash, bar, beta, between, bigcap, bigcup, bigvee, bigwedge, bmod, bot,
   braces, brackets, breve, cal, cap, cdots, cdotp, check, chi, circ, cong, cos, cosh,
   cot, csc, cup, ddot, ddots, delta, det, diamond, dim, displaystyle, divide,
   dot, downarrow, emptyset, epsilon, eq, neq, equiv, eta, exists, forall_, frac,
   gamma, gcd, ge, geq, grave, group, hat, iff, imath, implies, in_, inf,
   infty, int, iota, jmath, kappa, lambda, langle, lbrace, lceiling, lcm,
   ldots, ldotp, le, leftarrow, leftrightarrow, leq, lfloor, lim, liminf, limsup, ln,
   log, lparen, mathBinOp, longleftarrow, longrightarrow, longleftrightarrow,
   mathBinOps, mathCmd, mathCmdArg, mathCmdArgs, mathCmdMathArg, mathCmdMathArgs, mathCmdsArg,
   mathDecl, mathGroup, allMathItems, allMathDecls, rawDecls, decl, decls,
   mathbb, mathbf, mathnormal, mathrm, mathsf, mathit, mathscr,
   mathcal, mathfrak, mathtt, max, mbox, min, mit, mleft, mediumspace,
   negthinspace, mod, models, mrat, mright, msup, thickspace,
   thinspace, mu, nabla, ne, neg, notin, nu, oint, omega, omicron, oplus, otimes,
   overbrace, overline, parenChar, parens, partial, phi, pi, pm, pmod, prec,
   prod, propto, psi, quad, rangle, rawMath, rawMathChar, rbrace, rceiling,
   rfloor, rho, rightarrow, rparen, scriptscriptstyle, scriptstyle, sec, sigma, sin, sinh,
   space, sqrt, sqrt', square, stackrel, sub, subset, subseteq, succ, sum, sup,
   supset, supseteq, tan, tanh, tau, text, textstyle, theta, tilde, times, to, top,
   underbrace, underline, uparrow, upsilon, varepsilon, varphi, vartheta, vdash,
   vdots, vec, vee, wedge, widehat, widetilde, xi, zeta,
   a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,
   _A, _B, _C, _D, _E, _F, _G, _H, _I, _J, _K, _L, _M, _N, _O, _P,
   _Q, _R, _S, _T, _U, _V, _W, _X, _Y, _Z,
   _Downarrow, _Uparrow,
   vartriangleright,phantom,

   -- reexports
   cells, cell, vline, hline, cline
  ) where

import Prelude hiding (sqrt, min, max, lcm, gcd, log, mod, tanh, cosh, tan, sinh,
                       sin, cos, succ, sum, pi, mapM)
import Data.Char
import Data.Maybe (fromMaybe)
import Data.Foldable (foldMap)
import Data.Traversable (sequenceA, mapM)
import Data.String
import qualified Data.IntMap as IntMap
import Control.Arrow
import Control.Applicative
import Control.Monad hiding (mapM)
import Control.Monad.Error (throwError)

import Language.LaTeX.Types
import Language.LaTeX.Builder.MonoidUtils
import Language.LaTeX.Builder (XChar, cell, cells, vline, hline, cline)
import qualified Language.LaTeX.Builder as B
import qualified Language.LaTeX.Builder.Internal as B

liftMath :: (MathItm -> MathItm) -> MathItem -> MathItem
liftMath fun = MathItem . liftM fun . mathItmM

liftMath2 :: (MathItm -> MathItm -> MathItm) -> MathItem -> MathItem -> MathItem
liftMath2 fun (MathItem aa) (MathItem bb) = MathItem (liftM2 fun aa bb)

group :: MathItem -> MathItem
group = liftMath MathGroup

-- | Same as 'group'
mathGroup :: MathItem -> MathItem
mathGroup = liftMath MathGroup

mathCmdArgs :: String -> [Arg AnyItem] -> MathItem
mathCmdArgs name args = MathItem $ MathCmdArgs name <$> mapM (mapM anyItmM) args

mathCmdMathArgs :: String -> [Arg MathItem] -> MathItem
mathCmdMathArgs name = mathCmdArgs name . (map . fmap) B.mathItem

mathCmdArg :: String -> AnyItem -> MathItem
mathCmdArg m1 m2 = mathCmdArgs m1 [B.mandatory m2]

mathCmdMathArg :: String -> MathItem -> MathItem
mathCmdMathArg name = mathCmdMathArgs name . pure . B.mandatory

mathDecl :: String -> MathDecl
mathDecl = pure . MathDcl

rawDecls :: [MathDecl] -> MathItem
rawDecls = MathItem . fmap MathDecls . sequenceA

decls :: [MathDecl] -> MathItem -> MathItem
decls ds itm = group (rawDecls ds ⊕ itm)

decl :: MathDecl -> MathItem -> MathItem
decl dcl = decls [dcl]

mathCmd :: String -> MathItem
mathCmd = MathItem . pure . (`MathCmdArgs` [])

mathBinOp :: String -> MathItem -> MathItem -> MathItem
mathBinOp = liftMath2 . MathBinOp

rawMath :: String -> MathItem
rawMath = MathItem . pure . RawMath

rawMathChar :: Char -> MathItem
rawMathChar = rawMath . ('{':) . (:"}")

mrat :: Rational -> MathItem
mrat = fromRational

sub, sup :: MathItem -> MathItem
sub = (rawMath "_" ⊕) . mathGroup
sup = (rawMath "^" ⊕) . mathGroup

frac, stackrel :: MathItem -> MathItem -> MathItem
frac m1 m2 = mathCmdMathArgs "frac" [B.mandatory m1,B.mandatory m2]
stackrel m1 m2 = mathCmdMathArgs "stackrel" [B.mandatory m1,B.mandatory m2]

sqrt :: MathItem -> MathItem
sqrt = mathCmdMathArg "sqrt"

sqrt' :: MathItem -> MathItem -> MathItem
sqrt' n1 m1 = mathCmdMathArgs "sqrt" [B.optional n1, B.mandatory m1]

phantom :: MathItem -> MathItem
phantom = mathCmdMathArg "phantom"

mleft, mright :: Char -> MathItem
mleft m1  = MathItem $ RawMath . ("\\left"  ⊕) <$> parenChar m1
mright m1 = MathItem $ RawMath . ("\\right" ⊕) <$> parenChar m1

between :: Char -> Char -> MathItem -> MathItem
between opening closing m1 = mleft opening ⊕ m1 ⊕ mright closing

parens, braces, brackets :: MathItem -> MathItem
parens   = between '(' ')'
braces   = between '{' '}'
brackets = between '[' ']'

parenChar :: Char -> LatexM String
parenChar m1 | m1 `elem` "([.])" = return [m1]
             | m1 == '{'         = return "\\{"
             | m1 == '}'         = return "\\}"
             | otherwise         = throwError $ "invalid parenthesis-like: " ++ show m1

-- NOTE: This command is defined in the amsmath package. It does conflict with
-- some other packages/classes like the JFP class.
-- Maybe this should be move to a Amsmath module
text :: LatexItem -> MathItem
text arg = mathCmdArgs "text" [B.packageDependency amsmath, B.mandatoryLatexItem arg]

mbox :: LatexItem -> MathItem
mbox arg = mathCmdArgs "mbox" [B.mandatoryLatexItem arg]

array :: [RowSpec MathItem] -> [Row MathItem] -> MathItem
array spec items = MathItem $ B.tabularLike MathArray (map (fmap mathItmM) spec)
                                                      (map (fmap mathItmM) items)

-- TODO equation

{- This chunk was extracted from Language.LaTeX.Builder -}
lbrace :: MathItem
lbrace = mathCmd "{"
rbrace :: MathItem
rbrace = mathCmd "}"
lparen :: MathItem
lparen = rawMathChar '('
rparen :: MathItem
rparen = rawMathChar ')'
space :: MathItem
space = mathCmd " "
at :: MathItem
at = mathCmd "@"
in_ :: MathItem
in_ = mathCmd "in"
forall_ :: MathItem
forall_ = mathCmd "forall"
thinspace :: MathItem
thinspace = mathCmd ","
negthinspace :: MathItem
negthinspace = mathCmd "!"

-- \: or \> in LaTeX
mediumspace :: MathItem
mediumspace = mathCmd ":"
thickspace :: MathItem
thickspace = mathCmd ";"
msup :: MathItem
msup = mathCmd "sup"
alpha :: MathItem
alpha = mathCmd "alpha"
beta :: MathItem
beta = mathCmd "beta"
chi :: MathItem
chi = mathCmd "chi"
-- Chi? (don't forget the charToMath table)
delta :: MathItem
delta = mathCmd "delta"
_Delta :: MathItem
_Delta = mathCmd "Delta"
epsilon :: MathItem
epsilon = mathCmd "epsilon"
varepsilon :: MathItem
varepsilon = mathCmd "varepsilon"
eta :: MathItem
eta = mathCmd "eta"
gamma :: MathItem
gamma = mathCmd "gamma"
_Gamma :: MathItem
_Gamma = mathCmd "Gamma"
iota :: MathItem
iota = mathCmd "iota"
kappa :: MathItem
kappa = mathCmd "kappa"
lambda :: MathItem
lambda = mathCmd "lambda"
_Lambda :: MathItem
_Lambda = mathCmd "Lambda"
mu :: MathItem
mu = mathCmd "mu"
nu :: MathItem
nu = mathCmd "nu"
omega :: MathItem
omega = mathCmd "omega"
_Omega :: MathItem
_Omega = mathCmd "Omega"
-- | In LaTeX math mode omicron is just 'o',
--   it is exported as omicron here as well just
--   for convinience.
omicron :: MathItem
omicron = mathCmd "o"
phi :: MathItem
phi = mathCmd "phi"
varphi :: MathItem
varphi = mathCmd "varphi"
_Phi :: MathItem
_Phi = mathCmd "Phi"
pi :: MathItem
pi = mathCmd "pi"
_Pi :: MathItem
_Pi = mathCmd "Pi"
psi :: MathItem
psi = mathCmd "psi"
rho :: MathItem
rho = mathCmd "rho"
sigma :: MathItem
sigma = mathCmd "sigma"
_Sigma :: MathItem
_Sigma = mathCmd "Sigma"
tau :: MathItem
tau = mathCmd "tau"
theta :: MathItem
theta = mathCmd "theta"
vartheta :: MathItem
vartheta = mathCmd "vartheta"
_Theta :: MathItem
_Theta = mathCmd "Theta"
upsilon :: MathItem
upsilon = mathCmd "upsilon"
xi :: MathItem
xi = mathCmd "xi"
_Xi :: MathItem
_Xi = mathCmd "Xi"
zeta :: MathItem
zeta = mathCmd "zeta"
backslash :: MathItem
backslash = mathCmd "backslash"
times :: MathItem
times = mathCmd "times"
divide :: MathItem
divide = mathCmd "divide"
circ :: MathItem
circ = mathCmd "circ"
oplus :: MathItem
oplus = mathCmd "oplus"
otimes :: MathItem
otimes = mathCmd "otimes"
sum :: MathItem
sum = mathCmd "sum"
prod :: MathItem
prod = mathCmd "prod"
wedge :: MathItem
wedge = mathCmd "wedge"
bigwedge :: MathItem
bigwedge = mathCmd "bigwedge"
vee :: MathItem
vee = mathCmd "vee"
bigvee :: MathItem
bigvee = mathCmd "bigvee"
cup :: MathItem
cup = mathCmd "cup"
bigcup :: MathItem
bigcup = mathCmd "bigcup"
cap :: MathItem
cap = mathCmd "cap"
bigcap :: MathItem
bigcap = mathCmd "bigcap"
ne :: MathItem
ne = mathCmd "ne"
le :: MathItem
le = mathCmd "le"
leq :: MathItem
leq = mathCmd "leq"
ge :: MathItem
ge = mathCmd "ge"
geq :: MathItem
geq = mathCmd "geq"
prec :: MathItem
prec = mathCmd "prec"
succ :: MathItem
succ = mathCmd "succ"
notin :: MathItem
notin = mathCmd "notin"
subset :: MathItem
subset = mathCmd "subset"
supset :: MathItem
supset = mathCmd "supset"
subseteq :: MathItem
subseteq = mathCmd "subseteq"
supseteq :: MathItem
supseteq = mathCmd "supseteq"
equiv :: MathItem
equiv = mathCmd "equiv"
cong :: MathItem
cong = mathCmd "cong"
approx :: MathItem
approx = mathCmd "approx"
propto :: MathItem
propto = mathCmd "propto"
neg :: MathItem
neg = mathCmd "neg"
implies :: MathItem
implies = mathCmd "implies"
iff :: MathItem
iff = mathCmd "iff"
exists :: MathItem
exists = mathCmd "exists"
bot :: MathItem
bot = mathCmd "bot"
top :: MathItem
top = mathCmd "top"
vdash :: MathItem
vdash = mathCmd "vdash"
models :: MathItem
models = mathCmd "models"
langle :: MathItem
langle = mathCmd "langle"
rangle :: MathItem
rangle = mathCmd "rangle"
int :: MathItem
int = mathCmd "int"
oint :: MathItem
oint = mathCmd "oint"
partial :: MathItem
partial = mathCmd "partial"
nabla :: MathItem
nabla = mathCmd "nabla"
pm :: MathItem
pm = mathCmd "pm"
emptyset :: MathItem
emptyset = mathCmd "emptyset"
infty :: MathItem
infty = mathCmd "infty"
aleph :: MathItem
aleph = mathCmd "aleph"
ldots :: MathItem
ldots = mathCmd "ldots"
cdots :: MathItem
cdots = mathCmd "cdots"
vdots :: MathItem
vdots = mathCmd "vdots"
ddots :: MathItem
ddots = mathCmd "ddots"
cdotp :: MathItem
cdotp = mathCmd "cdotp"
ldotp :: MathItem
ldotp = mathCmd "ldotp"
quad :: MathItem
quad = mathCmd "quad"
diamond :: MathItem
diamond = mathCmd "diamond"
square :: MathItem
square = mathCmd "square"
lfloor :: MathItem
lfloor = mathCmd "lfloor"
rfloor :: MathItem
rfloor = mathCmd "rfloor"
lceiling :: MathItem
lceiling = mathCmd "lceiling"
rceiling :: MathItem
rceiling = mathCmd "rceiling"
sin :: MathItem
sin = mathCmd "sin"
cos :: MathItem
cos = mathCmd "cos"
tan :: MathItem
tan = mathCmd "tan"
csc :: MathItem
csc = mathCmd "csc"
sec :: MathItem
sec = mathCmd "sec"
cot :: MathItem
cot = mathCmd "cot"
sinh :: MathItem
sinh = mathCmd "sinh"
cosh :: MathItem
cosh = mathCmd "cosh"
tanh :: MathItem
tanh = mathCmd "tanh"
log :: MathItem
log = mathCmd "log"
ln :: MathItem
ln = mathCmd "ln"
det :: MathItem
det = mathCmd "det"
dim :: MathItem
dim = mathCmd "dim"
lim :: MathItem
lim = mathCmd "lim"
mod :: MathItem
mod = mathCmd "mod"
gcd :: MathItem
gcd = mathCmd "gcd"
lcm :: MathItem
lcm = mathCmd "lcm"
liminf :: MathItem
liminf = mathCmd "liminf"
inf :: MathItem
inf = mathCmd "inf"
limsup :: MathItem
limsup = mathCmd "limsup"
max :: MathItem
max = mathCmd "max"
min :: MathItem
min = mathCmd "min"
_Pr :: MathItem
_Pr = mathCmd "Pr"
uparrow :: MathItem
uparrow = mathCmd "uparrow"
downarrow :: MathItem
downarrow = mathCmd "downarrow"
_Uparrow :: MathItem
_Uparrow = mathCmd "Uparrow"
_Downarrow :: MathItem
_Downarrow = mathCmd "Downarrow"
rightarrow :: MathItem
rightarrow = mathCmd "rightarrow"
longrightarrow :: MathItem
longrightarrow = mathCmd "longrightarrow"
to :: MathItem
to = mathCmd "to"
leftarrow :: MathItem
leftarrow = mathCmd "leftarrow"
longleftarrow :: MathItem
longleftarrow = mathCmd "longleftarrow"
leftrightarrow :: MathItem
leftrightarrow = mathCmd "leftrightarrow"
longleftrightarrow :: MathItem
longleftrightarrow = mathCmd "longleftrightarrow"
_Rightarrow :: MathItem
_Rightarrow = mathCmd "Rightarrow"
_Leftarrow :: MathItem
_Leftarrow = mathCmd "Leftarrow"
_Leftrightarrow :: MathItem
_Leftrightarrow = mathCmd "Leftrightarrow"
-- https://secure.wikimedia.org/wikibooks/en/wiki/LaTeX/Mathematics#Formatting_mathematics_symbols
-- they are said to depend on amsfonts, maybe one should force it
mathnormal, mathrm, mathit, mathbf, mathsf, mathtt, mathbb, mathcal, mathfrak, mathscr :: MathItem -> MathItem
mathnormal = mathCmdMathArg "mathnormal"
mathrm = mathCmdMathArg "mathrm"
mathit = mathCmdMathArg "mathit"
mathbf = mathCmdMathArg "mathbf"
mathsf = mathCmdMathArg "mathsf"
mathtt = mathCmdMathArg "mathtt"
mathcal = mathCmdMathArg "mathcal"
mathfrak = mathCmdMathArg "mathfrak"
mathbb = mathCmdMathArg "mathbb"
mathscr = mathCmdMathArg "mathscr"
pmod :: MathItem -> MathItem
pmod = mathCmdMathArg "pmod"
tilde :: MathItem -> MathItem
tilde = mathCmdMathArg "tilde"
hat :: MathItem -> MathItem
hat = mathCmdMathArg "hat"
check :: MathItem -> MathItem
check = mathCmdMathArg "check"
breve :: MathItem -> MathItem
breve = mathCmdMathArg "breve"
acute :: MathItem -> MathItem
acute = mathCmdMathArg "acute"
grave :: MathItem -> MathItem
grave = mathCmdMathArg "grave"
bar :: MathItem -> MathItem
bar = mathCmdMathArg "bar"
vec :: MathItem -> MathItem
vec = mathCmdMathArg "vec"
dot :: MathItem -> MathItem
dot = mathCmdMathArg "dot"
ddot :: MathItem -> MathItem
ddot = mathCmdMathArg "ddot"
overbrace :: MathItem -> MathItem
overbrace = mathCmdMathArg "overbrace"
underbrace :: MathItem -> MathItem
underbrace = mathCmdMathArg "underbrace"
overline :: MathItem -> MathItem
overline = mathCmdMathArg "overline"
underline :: MathItem -> MathItem
underline = mathCmdMathArg "underline"
widehat :: MathItem -> MathItem
widehat = mathCmdMathArg "widehat"
widetilde :: MathItem -> MathItem
widetilde = mathCmdMathArg "widetilde"
imath :: MathItem -> MathItem
imath = mathCmdMathArg "imath"
jmath :: MathItem -> MathItem
jmath = mathCmdMathArg "jmath"
displaystyle :: MathDecl
displaystyle = mathDecl "displaystyle"
textstyle :: MathDecl
textstyle = mathDecl "textstyle"
scriptstyle :: MathDecl
scriptstyle = mathDecl "scriptstyle"
scriptscriptstyle :: MathDecl
scriptscriptstyle = mathDecl "scriptscriptstyle"
mit :: MathDecl
mit = mathDecl "mit"
cal :: MathDecl
cal = mathDecl "cal"
eq :: MathItem
eq = rawMathChar '='
neq :: MathItem
neq = mathCmd "neq"
a :: MathItem
a = rawMathChar 'a'
b :: MathItem
b = rawMathChar 'b'
c :: MathItem
c = rawMathChar 'c'
d :: MathItem
d = rawMathChar 'd'
e :: MathItem
e = rawMathChar 'e'
f :: MathItem
f = rawMathChar 'f'
g :: MathItem
g = rawMathChar 'g'
h :: MathItem
h = rawMathChar 'h'
i :: MathItem
i = rawMathChar 'i'
j :: MathItem
j = rawMathChar 'j'
k :: MathItem
k = rawMathChar 'k'
l :: MathItem
l = rawMathChar 'l'
m :: MathItem
m = rawMathChar 'm'
n :: MathItem
n = rawMathChar 'n'
o :: MathItem
o = rawMathChar 'o'
p :: MathItem
p = rawMathChar 'p'
q :: MathItem
q = rawMathChar 'q'
r :: MathItem
r = rawMathChar 'r'
s :: MathItem
s = rawMathChar 's'
t :: MathItem
t = rawMathChar 't'
u :: MathItem
u = rawMathChar 'u'
v :: MathItem
v = rawMathChar 'v'
w :: MathItem
w = rawMathChar 'w'
x :: MathItem
x = rawMathChar 'x'
y :: MathItem
y = rawMathChar 'y'
z :: MathItem
z = rawMathChar 'z'
_A :: MathItem
_A = rawMathChar 'A'
_B :: MathItem
_B = rawMathChar 'B'
_C :: MathItem
_C = rawMathChar 'C'
_D :: MathItem
_D = rawMathChar 'D'
_E :: MathItem
_E = rawMathChar 'E'
_F :: MathItem
_F = rawMathChar 'F'
_G :: MathItem
_G = rawMathChar 'G'
_H :: MathItem
_H = rawMathChar 'H'
_I :: MathItem
_I = rawMathChar 'I'
_J :: MathItem
_J = rawMathChar 'J'
_K :: MathItem
_K = rawMathChar 'K'
_L :: MathItem
_L = rawMathChar 'L'
_M :: MathItem
_M = rawMathChar 'M'
_N :: MathItem
_N = rawMathChar 'N'
_O :: MathItem
_O = rawMathChar 'O'
_P :: MathItem
_P = rawMathChar 'P'
_Q :: MathItem
_Q = rawMathChar 'Q'
_R :: MathItem
_R = rawMathChar 'R'
_S :: MathItem
_S = rawMathChar 'S'
_T :: MathItem
_T = rawMathChar 'T'
_U :: MathItem
_U = rawMathChar 'U'
_V :: MathItem
_V = rawMathChar 'V'
_W :: MathItem
_W = rawMathChar 'W'
_X :: MathItem
_X = rawMathChar 'X'
_Y :: MathItem
_Y = rawMathChar 'Y'
_Z :: MathItem
_Z = rawMathChar 'Z'

vartriangleright :: MathItem
vartriangleright = mathCmd "vartriangleright"

bmod :: MathItem -> MathItem -> MathItem
bmod = mathBinOp "bmod"

mathlift :: (LatexItem -> LatexItem) -> MathItem -> MathItem
mathlift fun = text . fun . B.math
{-# DEPRECATED mathlift "The use of M.text should be done with care (mbox is an alternative)" #-}

allMathDecls :: [MathDecl]
allMathDecls = [displaystyle, textstyle, scriptstyle, scriptscriptstyle, mit, cal]

allMathItems :: [MathItem]
allMathItems =
  [lbrace, rbrace, space, at, in_, forall_, thinspace, negthinspace, mediumspace,
   thickspace, msup, alpha, beta, chi, delta, _Delta, epsilon, varepsilon, eta,
   gamma, _Gamma, iota, kappa, lambda, _Lambda, mu, nu, omega, _Omega, phi, varphi,
   _Phi, pi, _Pi, psi, rho, sigma, _Sigma, tau, theta, vartheta, _Theta, upsilon,
   xi, _Xi, zeta, backslash, times, divide, circ, oplus, otimes, sum, prod, wedge,
   bigwedge, vee, bigvee, cup, bigcup, cap, bigcap, ne, le, leq, ge, geq, prec, succ,
   notin, subset, supset, subseteq, supseteq, equiv, cong, approx, propto, neg, implies,
   iff, exists, bot, top, vdash, models, langle, rangle, int, oint, partial, nabla, pm,
   emptyset, infty, aleph, ldots, cdots, vdots, ddots, quad, diamond, square, lfloor,
   rfloor, lceiling, rceiling, sin, cos, tan, csc, sec, cot, sinh, cosh, tanh, log, ln,
   det, dim, lim, mod, gcd, lcm, liminf, inf, limsup, max, min, _Pr, uparrow, downarrow,
   rightarrow, to, leftarrow, leftrightarrow, _Rightarrow, _Leftarrow, _Leftrightarrow
  ,longrightarrow,longrightarrow,longleftrightarrow
  ,vartriangleright, cdotp, ldotp, _Downarrow, _Uparrow
  -- maually added
  ,eq,neq
  ,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z
  ,_A, _B, _C, _D, _E, _F, _G, _H, _I, _J, _K, _L, _M, _N, _O, _P
  ,_Q, _R, _S, _T, _U, _V, _W, _X, _Y, _Z
  ,lparen,rparen
  ]

mathCmdsArg :: [MathItem -> MathItem]
mathCmdsArg = [mathbf, mathbb, mathcal, mathtt, mathfrak, pmod, tilde, hat, check,
                breve, acute, grave, bar, vec, dot, ddot, overbrace, underbrace, overline,
                underline, widehat, widetilde, imath, jmath
                -- maually added
               ,negate
               , sqrt, phantom
               ]

mathBinOps :: [MathItem -> MathItem -> MathItem]
mathBinOps = [(+),(-),(*),bmod]

amsmath :: PackageName
amsmath = PkgName "amsmath"

protector :: XChar -> String -> LatexItem
protector = B.protector . mchar

protect :: String -> LatexItem
protect = protector B.hchar

verb :: String -> LatexItem
verb = protector B.ttchar

{- NOT USED
mchar :: XChar
mchar '\\' = "\\textbackslash{}"
mchar '~'  = "\\text{\\~{}}"
mchar '^'  = "\\^{}"
mchar ':'  = ":"
mchar '_'  = "\\_"
mchar x | x `elem` "#&{}$%"  = ['\\',x]
        | x `elem` "]["      = ['{', x, '}'] -- to avoid mess up optional args
        | otherwise          = [x]
-}

type MXChar = Char -> MathItem

-- This char translator transformer only take care of math specific characters.
-- For instance 'a' will be displayed in text mode.
mchar :: XChar -> XChar
mchar xchar ch = maybe (xchar ch) B.math m'
  where m' | isAscii ch && isAlphaNum ch = Nothing
           | otherwise                   = charToMath ch

-- find a better name and export
mchar' :: XChar -> MXChar
mchar' xchar ch = fromMaybe (mbox (xchar ch)) (charToMath ch)

stringToMath :: String -> MathItem
stringToMath = foldMap $ mchar' B.hchar

mstring :: String -> LatexItem
mstring = B.math . stringToMath

instance IsString MathItem where
  fromString = stringToMath

charToMath :: Char -> Maybe MathItem
charToMath ch
   | isAscii ch && (isAlphaNum ch || isSpace ch) = Just $ rawMathChar ch
   | otherwise = IntMap.lookup (fromEnum ch) mapping
  where
    mapping = IntMap.fromList $ map (first fromEnum)
      [ ('α', alpha)
      , ('β', beta)
      , ('χ', chi)
      , ('δ', delta)
      , ('Δ', _Delta)
      , ('ε', epsilon)
      --, ('', varepsilon)
      , ('η', eta)
      , ('γ', gamma)
      , ('Γ', _Gamma)
      , ('ι', iota)
      , ('κ', kappa)
      , ('λ', lambda)
      , ('Λ', _Lambda)
      , ('μ', mu)
      , ('ν', nu)
      , ('ω', omega)
      , ('Ω', _Omega)
      , ('ο', omicron)
      , ('φ', phi)
      --, ('', varphi)
      , ('Φ', _Phi)
      , ('π', pi)
      , ('Π', _Pi)
      , ('ψ', psi)
      , ('ρ', rho)
      , ('σ', sigma)
      , ('Σ', _Sigma)
      , ('τ', tau)
      , ('θ', theta)
      --, ('', vartheta)
      , ('Θ', _Theta)
      --, ('', upsilon)
      , ('ξ', xi)
      --, ('', _Xi)
      , ('ζ', zeta)
      , ('×', times)
      , ('÷', divide)
      , ('·', cdotp)
      , ('∘', circ)
      , ('⊕', oplus)
      , ('⊛', otimes)
      , ('∧', wedge)
      --, ('', bigwedge)
      , ('∨', vee)
      --, ('', bigvee)
      , ('∪', cup)
      --, ('', bigcup)
      --, ('', cap)
      --, ('', bigcap)
      , ('≠', ne)
      --, ('', le)
      , ('≤', leq)
      --, ('', ge)
      , ('≥', geq)
      , ('∈', in_)
      , ('∉', notin)
      , ('⊂', subset)
      , ('⊃', supset)
      , ('⊆', subseteq)
      , ('⊇', supseteq)
      , ('≡', equiv)
      , ('=', eq)
      , ('≠', neq)
      --, ('', cong)
      , ('≈', approx)
      --, ('', propto)
      , ('¬', neg)
      --, ('', iff)
      , ('∀', forall_)
      , ('∃', exists)
      , ('⊥', bot)
      , ('⊤', top)
      , ('⊢', vdash)
      , ('⊩', models)
      {-
      , ('', langle)
      , ('', rangle)
      , ('', int)
      , ('', oint)
      , ('', partial)
      , ('', nabla)
      , ('', pm)
      -}
      , ('ø', emptyset)
      , ('∞', infty)
      , ('…', ldots)
      , ('⋯', cdots)
      {-
      , ('', aleph)
      , ('', vdots)
      , ('', ddots)
      , ('', quad)
      , ('', diamond)
      , ('', square)
      , ('', lfloor)
      , ('', rfloor)
      , ('', lceiling)
      , ('', rceiling)
      -}
      , ('↑', uparrow)
      {-
      , ('', downarrow)
prec
succ
      -}
      , ('→', rightarrow)
      --, ('', to)
      , ('←', leftarrow)
      , ('↔', leftrightarrow)
      , ('⇒', _Rightarrow)
      , ('⇐', _Leftarrow)
      , ('⇔', _Leftrightarrow)
      , ('⟶', longrightarrow)
      , ('⟵', longleftarrow)
      , ('▹', vartriangleright)
      -- black board
      , ('ℕ', mathbb _N)
      , ('ℍ', mathbb _H)
      , ('ℙ', mathbb _P)
      , ('ℝ', mathbb _R)
      , ('ⅅ', mathbb _D)
      , ('ℚ', mathbb _Q)
      , ('ℤ', mathbb _Z)
      --fail/TODO: find the correct definitions
      --, ('ℽ', mathbb gamma)
      --, ('ℾ', mathbb _Gamma)
      --, ('ℿ', mathbb _Pi)
      --, ('⅀', mathbb _Sigma)

      -- mathcal
      , ('℘', mathcal _P)
      , ('ℒ', mathcal _L)
      , ('ℛ', mathcal _R)
      , ('𝒩', mathcal _N)

      ]
