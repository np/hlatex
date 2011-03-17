{-# LANGUAGE TemplateHaskell #-}
module Language.LaTeX.Builder.QQ
  (-- * Quasi Quoters
   frQQ,frQQFile,str,strFile,istr,istrFile,tex,texFile,qm,qmFile,qp,qpFile,
   keys,keysFile,
   -- * Building new Quasi Quoters
   mkQQ,
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

frQQ = (quasiQuoter "frQQ"){ quoteExp = TH.stringE
                           , quotePat = TH.litP . TH.stringL }

str = frQQ

istr = (quasiQuoter "istr"){ quoteExp = TH.stringE . stripIdent }
  where stripIdent = unlines' . skipFirst (map (dropBar . dropWhile isSpace)) . lines
        unlines'   = intercalate "\n"
        skipFirst _ []     = []
        skipFirst f (x:xs) = x : f xs
        dropBar []       = []
        dropBar ('|':xs) = xs
        dropBar _        = error "istr: syntax error '|' expected after spaces"

mkQQ :: String -> TH.Name -> QuasiQuoter
mkQQ qqName qqFun = (quasiQuoter qqName){ quoteExp = TH.appE (TH.varE qqFun) . TH.stringE }

tex = mkQQ "tex" 'rawTex
qm  = mkQQ "qm"  'mstring
qp  = mkQQ "qp"  'rawPreamble

keys = (quasiQuoter "keys"){ quoteDec = fs } where
  fs = sequence . concatMap f . words
  clean = filter isAlphaNum
  f x = [TH.sigD n [t|Key|]
        ,TH.valD (TH.varP n)
                 (TH.normalB (TH.appE (TH.conE 'Key) $ TH.stringE x))
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
