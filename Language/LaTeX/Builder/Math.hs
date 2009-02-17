module Language.LaTeX.Builder.Math

  (_Delta, _Gamma, _Lambda, _Leftarrow, _Leftrightarrow, _Omega, _Phi, _Pi, _Pr,
   _Rightarrow, _Sigma, _Theta, _Xi, acute, aleph, alpha, approx, array, at,
   backslash, bar, beta, between, bigcap, bigcup, bigvee, bigwedge, bmod, bot,
   braces, brackets, breve, cal, cap, cdots, check, chi, circ, cong, cos, cosh,
   cot, csc, cup, ddot, ddots, delta, det, diamond, dim, displaystyle, divide,
   dot, downarrow, emptyset, epsilon, eq, equiv, eta, exists, forall_, frac,
   gamma, gcd, ge, geq, grave, group, hat, iff, imath, implies, in_, inf,
   infty, int, iota, jmath, kappa, lambda, langle, lbrace, lceiling, lcm,
   ldots, le, leftarrow, leftrightarrow, leq, lfloor, lim, liminf, limsup, ln,
   log, mathBinOp, mathBinOps, mathCmd, mathCmdArg, mathCmdArgs, mathCmdsArg,
   mathDecl, mathGroup, mathItems, mathNeedPackage, mathToLR, mathbb, mathbf,
   mathcal, mathfrak, mathtt, max, min, mit, mleft, mmediumspace,
   mnegthinspace, mod, models, mrat, mright, msup, mthickspace,
   mthinspace, mu, nabla, ne, neg, notin, nu, oint, omega, oplus, otimes,
   overbrace, overline, parenChar, parens, partial, phi, pi, pm, pmod, prec,
   prod, propto, psi, quad, rangle, rawMath, rawMathChar, rbrace, rceiling,
   rfloor, rho, rightarrow, scriptscriptstyle, scriptstyle, sec, sigma, sin, sinh,
   space, sqrt, sqrt', square, stackrel, sub, subset, subseteq, succ, sum, sup,
   supset, supseteq, tan, tanh, tau, text, textstyle, theta, tilde, times, to, top,
   underbrace, underline, uparrow, upsilon, varepsilon, varphi, vartheta, vdash,
   vdots, vec, vee, wedge, widehat, widetilde, xi, zeta,
   a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,
   _A, _B, _C, _D, _E, _F, _G, _H, _I, _J, _K, _L, _M, _N, _O, _P,
   _Q, _R, _S, _T, _U, _V, _W, _X, _Y, _Z,

   -- reexports
   cells, cell, vline, hline, cline
  ) where

import Prelude hiding (sqrt, min, max, lcm, gcd, log, mod, tanh, cosh, tan, sinh,
                       sin, cos, succ, sum, pi, mapM)
import Data.List hiding (sum, and, group)
import Data.Ratio
import Data.Char
import Data.Traversable (sequenceA, mapM)
import Control.Applicative hiding (optional)
import Control.Monad hiding (mapM)
import Control.Monad.Error (throwError)

import Language.LaTeX.Types
import Language.LaTeX.Builder.MonoidUtils
import Language.LaTeX.Builder (mandatory, optional, cell, cells, vline, hline, cline, tabularLike)
import qualified Language.LaTeX.Builder as B

group :: MathItem -> MathItem
group = liftM MathGroup

mathCmdArgs :: String -> [Arg MathItem] -> MathItem
mathCmdArgs m1 ys = MathCmdArgs m1 <$> mapM sequenceA ys

mathCmdArg :: String -> MathItem -> MathItem
mathCmdArg m1 m2 = mathCmdArgs m1 [mandatory m2]

mathDecl :: String -> MathItem
mathDecl = pure . MathDecl

mathCmd :: String -> MathItem
mathCmd = pure . (`MathCmdArgs` [])

mathBinOp :: String -> MathItem -> MathItem -> MathItem
mathBinOp = liftM2 . MathBinOp

rawMath :: String -> MathItem
rawMath = pure . RawMath

rawMathChar :: Char -> MathItem
rawMathChar = rawMath . (:[])

mathGroup :: MathItem -> MathItem
mathGroup = liftM MathGroup

mathNeedPackage :: String -> MathItem -> MathItem
mathNeedPackage = liftM . MathNeedPackage

mathToLR :: String -> LatexItem -> MathItem
mathToLR = liftM . MathToLR

mrat :: Rational -> MathItem
mrat = pure . fromRational

sub, sup :: MathItem -> MathItem
sub = (rawMath "_" <>) . mathGroup
sup = (rawMath "^" <>) . mathGroup

frac, stackrel :: MathItem -> MathItem -> MathItem
frac m1 m2 = mathCmdArgs "frac" [mandatory m1,mandatory m2]
stackrel m1 m2 = mathCmdArgs "stackrel" [mandatory m1,mandatory m2]

sqrt :: MathItem -> MathItem
sqrt = mathCmdArgs "sqrt" . (:[]) . mandatory

sqrt' :: MathItem -> MathItem -> MathItem
sqrt' n1 m1 = mathCmdArgs "sqrt" [optional n1, mandatory m1]

mleft, mright :: Char -> MathItem
mleft m1  = rawMath "\\left"  <> (RawMath <$> parenChar m1)
mright m1 = rawMath "\\right" <> (RawMath <$> parenChar m1)

between :: Char -> Char -> MathItem -> MathItem
between opening closing m1 = mleft opening <> m1 <> mright closing

parens, braces, brackets :: MathItem -> MathItem
parens   = between '(' ')'
braces   = between '{' '}'
brackets = between '[' ']'

parenChar :: Char -> LatexM String
parenChar m1 | m1 `elem` "([.])" = return [m1]
             | m1 == '{'         = return "\\{"
             | m1 == '}'         = return "\\}"
             | otherwise        = throwError $ "invalid parenthesis-like: " ++ show m1

text :: LatexItem -> MathItem
text = mathNeedPackage "amsmath" . mathToLR "text"

array :: [RowSpec MathItem] -> [Row MathItem] -> MathItem
array = B.tabularLike MathArray

{- This chunk was extracted from Language.LaTeX.Builder -}
lbrace :: MathItem
lbrace = mathCmd "{"
rbrace :: MathItem
rbrace = mathCmd "}"
space :: MathItem
space = mathCmd " "
at :: MathItem
at = mathCmd "@"
in_ :: MathItem
in_ = mathCmd "in"
forall_ :: MathItem
forall_ = mathCmd "forall"
mthinspace :: MathItem
mthinspace = mathCmd ","
mnegthinspace :: MathItem
mnegthinspace = mathCmd "!"
mmediumspace :: MathItem
mmediumspace = mathCmd ":"
mthickspace :: MathItem
mthickspace = mathCmd ";"
msup :: MathItem
msup = mathCmd "sup"
alpha :: MathItem
alpha = mathCmd "alpha"
beta :: MathItem
beta = mathCmd "beta"
chi :: MathItem
chi = mathCmd "chi"
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
rightarrow :: MathItem
rightarrow = mathCmd "rightarrow"
to :: MathItem
to = mathCmd "to"
leftarrow :: MathItem
leftarrow = mathCmd "leftarrow"
leftrightarrow :: MathItem
leftrightarrow = mathCmd "leftrightarrow"
_Rightarrow :: MathItem
_Rightarrow = mathCmd "Rightarrow"
_Leftarrow :: MathItem
_Leftarrow = mathCmd "Leftarrow"
_Leftrightarrow :: MathItem
_Leftrightarrow = mathCmd "Leftrightarrow"
mathbf :: MathItem -> MathItem
mathbf = mathCmdArg "mathbf"
mathbb :: MathItem -> MathItem
mathbb = mathCmdArg "mathbb"
mathcal :: MathItem -> MathItem
mathcal = mathCmdArg "mathcal"
mathtt :: MathItem -> MathItem
mathtt = mathCmdArg "mathtt"
mathfrak :: MathItem -> MathItem
mathfrak = mathCmdArg "mathfrak"
pmod :: MathItem -> MathItem
pmod = mathCmdArg "pmod"
tilde :: MathItem -> MathItem
tilde = mathCmdArg "tilde"
hat :: MathItem -> MathItem
hat = mathCmdArg "hat"
check :: MathItem -> MathItem
check = mathCmdArg "check"
breve :: MathItem -> MathItem
breve = mathCmdArg "breve"
acute :: MathItem -> MathItem
acute = mathCmdArg "acute"
grave :: MathItem -> MathItem
grave = mathCmdArg "grave"
bar :: MathItem -> MathItem
bar = mathCmdArg "bar"
vec :: MathItem -> MathItem
vec = mathCmdArg "vec"
dot :: MathItem -> MathItem
dot = mathCmdArg "dot"
ddot :: MathItem -> MathItem
ddot = mathCmdArg "ddot"
overbrace :: MathItem -> MathItem
overbrace = mathCmdArg "overbrace"
underbrace :: MathItem -> MathItem
underbrace = mathCmdArg "underbrace"
overline :: MathItem -> MathItem
overline = mathCmdArg "overline"
underline :: MathItem -> MathItem
underline = mathCmdArg "underline"
widehat :: MathItem -> MathItem
widehat = mathCmdArg "widehat"
widetilde :: MathItem -> MathItem
widetilde = mathCmdArg "widetilde"
imath :: MathItem -> MathItem
imath = mathCmdArg "imath"
jmath :: MathItem -> MathItem
jmath = mathCmdArg "jmath"
displaystyle :: MathItem
displaystyle = mathDecl "displaystyle"
textstyle :: MathItem
textstyle = mathDecl "textstyle"
scriptstyle :: MathItem
scriptstyle = mathDecl "scriptstyle"
scriptscriptstyle :: MathItem
scriptscriptstyle = mathDecl "scriptscriptstyle"
mit :: MathItem
mit = mathDecl "mit"
cal :: MathItem
cal = mathDecl "cal"
eq :: MathItem
eq = rawMath "{=}"
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

bmod :: MathItem -> MathItem -> MathItem
bmod = mathBinOp "bmod"

mathItems :: [MathItem]
mathItems =
  [lbrace, rbrace, space, at, in_, forall_, mthinspace, mnegthinspace, mmediumspace,
   mthickspace, msup, alpha, beta, chi, delta, _Delta, epsilon, varepsilon, eta,
   gamma, _Gamma, iota, kappa, lambda, _Lambda, mu, nu, omega, _Omega, phi, varphi,
   _Phi, pi, _Pi, psi, rho, sigma, _Sigma, tau, theta, vartheta, _Theta, upsilon,
   xi, _Xi, zeta, backslash, times, divide, circ, oplus, otimes, sum, prod, wedge,
   bigwedge, vee, bigvee, cup, bigcup, cap, bigcap, ne, le, leq, ge, geq, prec, succ,
   notin, subset, supset, subseteq, supseteq, equiv, cong, approx, propto, neg, implies,
   iff, exists, bot, top, vdash, models, langle, rangle, int, oint, partial, nabla, pm,
   emptyset, infty, aleph, ldots, cdots, vdots, ddots, quad, diamond, square, lfloor,
   rfloor, lceiling, rceiling, sin, cos, tan, csc, sec, cot, sinh, cosh, tanh, log, ln,
   det, dim, lim, mod, gcd, lcm, liminf, inf, limsup, max, min, _Pr, uparrow, downarrow,
   rightarrow, to, leftarrow, leftrightarrow, _Rightarrow, _Leftarrow, _Leftrightarrow,
   displaystyle, textstyle, scriptstyle, scriptscriptstyle, mit, cal
  -- maually added
  ,eq
  ,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z
  ,_A, _B, _C, _D, _E, _F, _G, _H, _I, _J, _K, _L, _M, _N, _O, _P
  ,_Q, _R, _S, _T, _U, _V, _W, _X, _Y, _Z
  ]

mathCmdsArg :: [MathItem -> MathItem]
mathCmdsArg = [mathbf, mathbb, mathcal, mathtt, mathfrak, pmod, tilde, hat, check,
                breve, acute, grave, bar, vec, dot, ddot, overbrace, underbrace, overline,
                underline, widehat, widetilde, imath, jmath
                -- maually added
               ,negate
               , sqrt
               ]

mathBinOps :: [MathItem -> MathItem -> MathItem]
mathBinOps = [(+),(-),(*),bmod]

