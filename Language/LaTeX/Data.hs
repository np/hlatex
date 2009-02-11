module Language.LaTeX.Data where

import Data.Char
import Control.Arrow
import Language.Haskell.TH

mathsCmdsArg, texDecls, latexCmds :: [String]
mathsCmds :: [(String, String)]

mathsCmds =
  [("lbrace", "{")
  ,("rbrace", "}")
  ,("space", " ")
  ,("at", "@")
  ,("in_", "in")
  ,("forall_", "forall")
  ] ++ map (id &&& id)
  [-- Greek letters
   "alpha","beta","chi","delta","Delta","epsilon","varepsilon","eta","gamma"
  ,"Gamma","iota","kappa","lambda","Lambda","mu","nu","omega","Omega","phi"
  ,"varphi","Phi","pi","Pi","psi","rho","sigma","Sigma","tau","theta"
  ,"vartheta","Theta","upsilon","xi","Xi","zeta"

  -- Operation symbols
  ,"backslash","times","divide","circ","oplus","otimes","sum","prod","wedge"
  ,"bigwedge","vee","bigvee","cup","bigcup","cap","bigcap"

  -- Relation symbols
  ,"ne","le","leq","ge","geq","prec","succ","notin","subset","supset"
  ,"subseteq","supseteq","equiv","cong","approx","propto"

  -- Logical symbols
  ,"neg","implies","iff","exists","bot","top","vdash","models"

  -- Grouping brackets
  ,"langle","rangle"

  -- Miscellaneous symbols
  ,"int","oint","partial","nabla","pm","emptyset","infty","aleph","ldots"
  ,"cdots","quad","diamond","square","lfloor","rfloor","lceiling","rceiling"

  -- Standard functions
  ,"sin","cos","tan","csc","sec","cot","sinh","cosh","tanh","log"
  ,"ln","det","dim","lim","mod","gcd","lcm"

  -- Arrows
  ,"uparrow","downarrow","rightarrow","to","leftarrow"
  ,"leftrightarrow","Rightarrow","Leftarrow","Leftrightarrow"
  ]

mathsCmdsArg =
  [-- Font commands
   "mathbf","mathbb","mathcal","mathtt","mathfrak"
  ]

typeStyles = ["em","bf","sf","sl","sc","it","tt"]

texDecls = typeStyles

latexCmds = map ("text"++) typeStyles

lowerName name | isLower (head name) = mkName name
               | otherwise           = mkName $ '_':name
