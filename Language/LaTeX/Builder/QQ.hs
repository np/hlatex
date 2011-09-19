{-# LANGUAGE TemplateHaskell #-}
module Language.LaTeX.Builder.QQ
  (-- * Quasi Quoters
   frQQ,frQQFile,str,strFile,istr,istrFile,tex,texFile,qm,qmFile,qp,qpFile,
   keys,keysFile,
   -- * Building new Quasi Quoters
   mkQQ, mkQQnoIndent,
   stripIdentQQ,
   -- * Misc functions used by the frquotes expander of «...»
   frTop, frAntiq,
  ) where

import Data.List
import Data.Char
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
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
  where err kind _ = error $ qqName ++ ": not available in " ++ kind

stripIdentQQ :: String -> String
stripIdentQQ = unlines' . skipFirst (map dropBar . dropLastWhen null . map (dropWhile isSpace)) . lines
  where unlines'   = intercalate "\n"
        skipFirst _ []     = []
        skipFirst f (x:xs) = x : f xs
        dropLastWhen _ [] = []
        dropLastWhen p (x:xs) | null xs && p x = []
                              | otherwise      = x:dropLastWhen p xs
        dropBar ('|':xs) = xs
        dropBar []       = error "stripIdentQQ: syntax error '|' expected after spaces (unexpected empty string)"
        dropBar (c:xs)   = error $ "stripIdentQQ: syntax error '|' expected after spaces (unexpected "++show c++")"

str = (quasiQuoter "str"){ quoteExp = TH.stringE
                         , quotePat = TH.litP . TH.stringL }

mkQQnoIndent :: String -> TH.Name -> QuasiQuoter
mkQQnoIndent qqName qqFun = (quasiQuoter qqName){ quoteExp = TH.appE (TH.varE qqFun) . TH.stringE }

mkQQ :: String -> TH.Name -> QuasiQuoter
mkQQ qqName qqFun = (quasiQuoter qqName){ quoteExp = TH.appE (TH.varE qqFun) . TH.stringE . stripIdentQQ }

-- istr ≡ mkQQ "istr" 'id
istr = (quasiQuoter "istr"){ quoteExp = TH.stringE . stripIdentQQ }

frQQ = mkQQnoIndent "frQQ" 'hstring
tex  = mkQQ "tex"  'rawTex
qm   = mkQQ "qm"   'mstring
qp   = mkQQ "qp"   'rawPreamble

keys = (quasiQuoter "keys"){ quoteDec = fs } where
  fs = sequence . concatMap f . words
  clean = filter isAlphaNum
  f x = [TH.sigD n [t|Key|]
        ,TH.valD (TH.varP n)
                 (TH.normalB (TH.appE (TH.conE 'MkKey) $ TH.stringE x))
                 []
        ]
        where n = TH.mkName (clean x)

frQQFile  = quoteFile frQQ
strFile   = quoteFile str
istrFile  = quoteFile istr
texFile   = quoteFile tex
qmFile    = quoteFile qm
qpFile    = quoteFile qp
keysFile  = quoteFile keys
