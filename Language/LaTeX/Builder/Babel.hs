-- TODO add more langs
module Language.LaTeX.Builder.Babel
   (Lang, BabelOpt
   ,useBabel
   ,langName
   -- langs
   ,francais
   ,french
   -- last resort
   ,customLang
   ,customBabelOpt
   ,pkg
   ) where

import Data.String
import Language.LaTeX.Types
import qualified Language.LaTeX.Builder.Internal as BI

newtype BabelOpt = BabelOpt { babelOpt :: LatexItem }

newtype Lang = Lang { langName :: String }
  deriving (Show, Eq)

francais, french :: Lang
francais = Lang "francais"
french   = Lang "french"

customLang :: String -> Lang
customLang = Lang

customBabelOpt :: LatexItem -> BabelOpt
customBabelOpt = BabelOpt

pkg :: PackageName
pkg = BI.pkgName "babel"

useBabel :: Lang -> [BabelOpt] -> PreambleItem
useBabel lang opts = BI.usepackage (fromString (langName lang)
                                   : map babelOpt opts) pkg
