{-# LANGUAGE TemplateHaskell #-}
module Language.LaTeX.Builder.QQ (frTop, frAntiq, frQQ, tex, str, istr) where

import Data.List
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Language.LaTeX.Builder (rawTex)

frTop :: a -> a
frTop = id

frAntiq :: a -> a
frAntiq = id

frQQ,str,istr,tex :: QuasiQuoter

frQQ = QuasiQuoter TH.stringE (TH.litP . TH.stringL)
str = frQQ

istr = QuasiQuoter (TH.stringE . stripIdent) (error "istr: not available in patterns")
  where stripIdent = unlines' . skipFirst (map (drop 1 . dropWhile (/='|'))) . lines
        unlines'   = intercalate "\n"
        skipFirst _ []     = []
        skipFirst f (x:xs) = x : f xs

tex = QuasiQuoter (TH.appE (TH.varE 'rawTex) . TH.stringE) (error "tex: not available in patterns")

