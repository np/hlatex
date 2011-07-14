module Language.LaTeX.Builder.Rotating (pkg, turn) where

import Language.LaTeX.Types
import qualified Language.LaTeX.Builder.Internal as BI

pkg :: PackageName
pkg = BI.pkgName "rotating"

turn :: Int -> LatexItem -> LatexItem
turn i = BI.latexEnvironment "turn" [BI.packageDependency pkg, BI.mandatory (BI.num i)]
