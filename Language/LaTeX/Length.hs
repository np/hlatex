module Language.LaTeX.Length
  (
  -- Units
    inch, pt, em, cm, mm, ex, pc, sp, bp, dd, cc, mu
  -- Dynamic Length
  , stretch
  -- Predefined lengths
  , parindent, textwidth, linewidth, textheight, parsep, parskip, baselineskip
  , baselinestrech, fill, columnsep, columnseprule, mathindent, oddsidemargin
  , evensidemargin, marginparwidth, marginparsep, marginparpush, topmargin
  , headheight, headsep, topskip, footheight, footskip, topsep, partopsep, itemsep
  , itemindent, labelsep, labelwidth, leftmargin, rightmargin, listparindent, jot
  , abovedisplayskip, belowdisplayskip, abovedisplayshortskip
  , belowdisplayshortskip, floatsep, textfloatsep, intextsep, dblfloatsep
  , dbltextfloatsep, textfraction, floatpagefraction, dbltopfaction
  , dblfloatpagefraction, arraycolsep, tabcolsep, arrayrulewidth, doublerulesep
  , arraystretch, bigskipamount, medskipamount, smallskipamount, fboxrule, fboxsep
  )
  where

import Language.LaTeX.Types

inch, pt, em, cm, mm, ex, pc, sp, bp, dd, cc, mu :: Rational -> LatexLength
pt = withUnit Pt
em = withUnit Em
cm = withUnit Cm
mm = withUnit Mm
ex = withUnit Ex
pc = withUnit Pc

-- | Since 'in' is a keyword in Haskell, this one is called 'inch'.
inch = withUnit In
sp = withUnit Sp
bp = withUnit Bp
dd = withUnit Dd
cc = withUnit Cc
mu = withUnit Mu

-- | Internal function to make LatexLength commands
lengthCmd :: String -> LatexLength
lengthCmd = LengthCmd

-- | Internal function to make LatexLength commands
withUnit :: TexUnit -> Rational -> LatexLength
withUnit unit = LengthCst (Just unit)

-- robust
stretch :: Rational -> LatexLength
stretch = LengthCmdRatArg "stretch"

parindent, textwidth, linewidth, textheight, parsep, parskip, baselineskip, baselinestrech,
  fill, columnsep, columnseprule, mathindent, oddsidemargin, evensidemargin, marginparwidth,
  marginparsep, marginparpush, topmargin, headheight, headsep, topskip, footheight, footskip,
  topsep, partopsep, itemsep, itemindent, labelsep, labelwidth, leftmargin, rightmargin,
  listparindent, jot, abovedisplayskip, belowdisplayskip, abovedisplayshortskip,
  belowdisplayshortskip, floatsep, textfloatsep, intextsep, dblfloatsep, dbltextfloatsep,
  textfraction, floatpagefraction, dbltopfaction, dblfloatpagefraction, arraycolsep,
  tabcolsep, arrayrulewidth, doublerulesep, arraystretch, bigskipamount, medskipamount,
  smallskipamount, fboxrule, fboxsep :: LatexLength

parindent = lengthCmd "parindent"
textwidth = lengthCmd "textwidth"
linewidth = lengthCmd "linewidth"
textheight = lengthCmd "textheight"
parsep = lengthCmd "parsep"
parskip = lengthCmd "parskip"
baselineskip = lengthCmd "baselineskip"
baselinestrech = lengthCmd "baselinestrech"
fill = lengthCmd "fill"
columnsep = lengthCmd "columnsep"
columnseprule = lengthCmd "columnseprule"
mathindent = lengthCmd "mathindent"
oddsidemargin = lengthCmd "oddsidemargin"
evensidemargin = lengthCmd "evensidemargin"
marginparwidth = lengthCmd "marginparwidth"
marginparsep = lengthCmd "marginparsep"
marginparpush = lengthCmd "marginparpush"
topmargin = lengthCmd "topmargin"
headheight = lengthCmd "headheight"
headsep = lengthCmd "headsep"
topskip = lengthCmd "topskip"
footheight = lengthCmd "footheight"
footskip = lengthCmd "footskip"
topsep = lengthCmd "topsep"
partopsep = lengthCmd "partopsep"
itemsep = lengthCmd "itemsep"
itemindent = lengthCmd "itemindent"
labelsep = lengthCmd "labelsep"
labelwidth = lengthCmd "labelwidth"
leftmargin = lengthCmd "leftmargin"
rightmargin = lengthCmd "rightmargin"
listparindent = lengthCmd "listparindent"
jot = lengthCmd "jot"
abovedisplayskip = lengthCmd "abovedisplayskip"
belowdisplayskip = lengthCmd "belowdisplayskip"
abovedisplayshortskip = lengthCmd "abovedisplayshortskip"
belowdisplayshortskip = lengthCmd "belowdisplayshortskip"
floatsep = lengthCmd "floatsep"
textfloatsep = lengthCmd "textfloatsep"
intextsep = lengthCmd "intextsep"
dblfloatsep = lengthCmd "dblfloatsep"
dbltextfloatsep = lengthCmd "dbltextfloatsep"
textfraction = lengthCmd "textfraction"
floatpagefraction = lengthCmd "floatpagefraction"
dbltopfaction = lengthCmd "dbltopfaction"
dblfloatpagefraction = lengthCmd "dblfloatpagefraction"
arraycolsep = lengthCmd "arraycolsep"
tabcolsep = lengthCmd "tabcolsep"
arrayrulewidth = lengthCmd "arrayrulewidth"
doublerulesep = lengthCmd "doublerulesep"
arraystretch = lengthCmd "arraystretch"
bigskipamount = lengthCmd "bigskipamount"
medskipamount = lengthCmd "medskipamount"
smallskipamount = lengthCmd "smallskipamount"
fboxrule = lengthCmd "fboxrule"
fboxsep = lengthCmd "fboxsep"

