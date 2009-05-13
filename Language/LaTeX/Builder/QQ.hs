{-# LANGUAGE TemplateHaskell #-}
module Language.LaTeX.Builder.QQ (frTop, frAntiq, frQQ, tex, str) where

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Language.LaTeX.Builder (rawTex)

frTop :: a -> a
frTop = id

frAntiq :: a -> a
frAntiq = id

frQQ,str :: QuasiQuoter
frQQ = QuasiQuoter TH.stringE (TH.litP . TH.stringL)
str = frQQ

tex :: QuasiQuoter
tex = QuasiQuoter (TH.appE (TH.varE 'rawTex) . TH.stringE) (error "tex: not available in patterns")

