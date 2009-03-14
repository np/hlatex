module Language.LaTeX.Builder.Rotating (pkg, turn) where

import Language.LaTeX.Types
import qualified Language.LaTeX.Builder as B

pkg :: PackageName
pkg = B.pkgName "rotating"

turn :: Int -> LatexItem -> LatexItem
turn i x = B.latexEnvironment "turn" [B.packageDependency pkg, B.mandatory (B.num i)] x