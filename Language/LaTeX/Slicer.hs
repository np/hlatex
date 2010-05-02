module Language.LaTeX.Slicer
  (marknote, mark, marker, (^$), slice)
where

import Data.Monoid
import Data.Maybe
import Data.List (find)
import Control.Monad.Writer
import Language.LaTeX.Types
import qualified Data.Generics.Uniplate.Data as U

marknote :: ParItm
marknote = ParNote (TextNote "slicemark") mempty

mark :: ParItemW
mark = tell $ return marknote

marker :: Writer ParItem a -> Writer ParItem a
marker f = do mark
              x <- f
              mark
              return x

infixr 0 ^$
(^$) :: (b -> Writer ParItem a) -> b -> Writer ParItem a
f ^$ x = marker (f x)

slice :: Functor f => f ParItm -> f ParItm
slice = fmap $ mconcat . comb [maySlice1, maySlice2] . uncatParItm
  where notMark = (/= marknote)
        isMarked = any (== marknote) . U.universe
        maySlice1 = takeWhile notMark . drop 1 . dropWhile notMark
        maySlice2 = filter isMarked
        comb fs xs = fromMaybe xs . find (not . null) $ map ($xs) fs
