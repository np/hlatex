{-# LANGUAGE TemplateHaskell #-}
module Language.LaTeX.Builder.QQ (frTop, frAntiq, frQQ, tex, qp, str, istr, qm) where

import Data.List
import Data.Char
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Language.LaTeX.Builder.Internal (rawTex, rawPreamble)
import Language.LaTeX.Builder.Math (mstring)

frTop :: a -> a
frTop = id

frAntiq :: a -> a
frAntiq = id

frQQ,str,istr,tex,qm,qp :: QuasiQuoter

frQQ = QuasiQuoter TH.stringE (TH.litP . TH.stringL)
str = frQQ

istr = QuasiQuoter (TH.stringE . stripIdent) (error "istr: not available in patterns")
  where stripIdent = unlines' . skipFirst (map (dropBar . dropWhile isSpace)) . lines
        unlines'   = intercalate "\n"
        skipFirst _ []     = []
        skipFirst f (x:xs) = x : f xs
        dropBar []       = []
        dropBar ('|':xs) = xs
        dropBar _        = error "istr: syntax error '|' expected after spaces"

tex = QuasiQuoter (TH.appE (TH.varE 'rawTex) . TH.stringE) (error "tex: not available in patterns")

qm = QuasiQuoter (TH.appE (TH.varE 'mstring) . TH.stringE) (error "qm: not available in patterns")

qp = QuasiQuoter (TH.appE (TH.varE 'rawPreamble) . TH.stringE) (error "qm: not available in patterns")
