{-# LANGUAGE TemplateHaskell #-}
module Language.LaTeX.Builder.QQ (frTop, frAntiq, frQQ) where

-- import Data.String
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote

frTop :: a -> a
frTop = id

frAntiq :: a -> a
frAntiq = id

frQQ :: QuasiQuoter
frQQ = QuasiQuoter TH.stringE -- (\str-> TH.varE 'fromString `TH.appE` TH.stringE str)
                   (error "frQQ: not available in patterns")


