{-# LANGUAGE TemplateHaskell #-}
module Language.LaTeX.Builder.QQ
  (-- * Quasi Quoters
   frQQ,frQQFile,str,strFile,istr,istrFile,tex,texFile,qm,qmFile,qp,qpFile,
   keys,keysFile,
   -- * Building new Quasi Quoters
   mkQQ, mkQQnoIndent, mkQQgen,
   stripIndentQQ,
   -- * Misc functions used by the frquotes expander of «...»
   frTop, frAntiq,
  ) where

import Data.List
import Data.Char
import Data.Functor
import Language.Haskell.TH (Q, Exp, Name, appE, varE, stringE, litP, stringL, valD,
                            varP, sigD, mkName, normalB, conE)
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax (Lift(..))
import Language.LaTeX.Types (Key(..))
import Language.LaTeX.Builder.Internal (rawTex, rawPreamble)
import Language.LaTeX.Builder.Math (mstring)
import Language.LaTeX.Builder (hstring)

frTop :: a -> a
frTop = id

frAntiq :: a -> a
frAntiq = id

frQQ,frQQFile,str,strFile,istr,istrFile,tex,texFile,qm,qmFile,qp,qpFile,
  keys, keysFile :: QuasiQuoter

quasiQuoter :: String -> QuasiQuoter
quasiQuoter qqName =
  QuasiQuoter (err "expressions") (err "patterns")
-- if GHC7
              (err "types") (err "declarations")
-- endif
  where err kind _ = fail $ qqName ++ ": not available in " ++ kind

stripIndentQQ :: String -> Q String
stripIndentQQ = fmap unlines' . skipFirst (mapM dropBar . dropLastWhen null . map (dropWhile isSpace)) . lines
  where unlines'   = intercalate "\n"
        skipFirst _ []     = return []
        skipFirst f (x:xs) = (x :) <$> f xs
        dropLastWhen _ [] = []
        dropLastWhen p (x:xs) | null xs && p x = []
                              | otherwise      = x:dropLastWhen p xs
        dropBar ('|':xs) = return xs
        dropBar []       = fail "stripIndentQQ: syntax error '|' expected after spaces (unexpected empty string)"
        dropBar (c:_)    = fail $ "stripIndentQQ: syntax error '|' expected after spaces (unexpected "++show c++")"

str = (quasiQuoter "str"){ quoteExp = stringE
                         , quotePat = litP . stringL }

mkQQgen :: (String -> Q Exp) -> String -> Name -> QuasiQuoter
mkQQgen pre qqName qqFun = (quasiQuoter qqName){ quoteExp = appE (varE qqFun) . pre }

mkQQ :: String -> Name -> QuasiQuoter
mkQQ = mkQQgen ((lift =<<) . stripIndentQQ)

mkQQnoIndent :: String -> Name -> QuasiQuoter
mkQQnoIndent = mkQQgen lift

-- istr ≡ mkQQ "istr" 'id
istr = (quasiQuoter "istr"){ quoteExp = (stringE =<<) . stripIndentQQ }

frQQ = mkQQnoIndent "frQQ" 'hstring
tex  = mkQQ "tex"  'rawTex
qm   = mkQQ "qm"   'mstring
qp   = mkQQ "qp"   'rawPreamble

keys = (quasiQuoter "keys"){ quoteDec = fs } where
  fs = sequence . concatMap f . words
  clean = filter isAlphaNum
  f x = [sigD n [t|Key|]
        ,valD (varP n)
              (normalB (appE (conE 'MkKey) $ stringE x))
              []
        ]
        where n = mkName (clean x)

frQQFile  = quoteFile frQQ
strFile   = quoteFile str
istrFile  = quoteFile istr
texFile   = quoteFile tex
qmFile    = quoteFile qm
qpFile    = quoteFile qp
keysFile  = quoteFile keys
