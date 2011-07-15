module Language.LaTeX.Checker where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Generics.Uniplate.Data (universeBi)
import Language.LaTeX.Types

providedPackages :: PreambleItm -> Set PackageName
providedPackages x = Set.fromList [ pkg | ProvidePackage pkg <- universeBi x ]

neededPackages :: Document -> Set PackageName
neededPackages x = Set.fromList [ pkg | PackageDependency pkg <- universeBi x ]

checkDocument :: Document -> Maybe ErrorMessage
checkDocument doc
  | not . null $ missingPkgs = Just ("Missing packages: " ++ show missingPkgs)
  | otherwise                = Nothing
 where providedPkgs = providedPackages . documentPreamble $ doc
       neededPkgs   = neededPackages doc
       missingPkgs  = map getPkgName . Set.toList $ neededPkgs `Set.difference` providedPkgs
