module Language.LaTeX.Slicer
  (marknote, mark, marker, (^$), slice)
where

import Control.Monad.Writer
import Language.LaTeX.Types
import Language.LaTeX.Builder.MonoidUtils
import qualified Language.LaTeX.Builder.Internal as BI
import qualified Data.Generics.Uniplate.Data as U

marknote :: ParItm
marknote = ParNote (MkKey "slicemark") BI.nilNote ø

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
slice = fmap $ mconcat . mconcat . comb [maySlice1, maySlice2, id] . uncatParItm
  where notMark = (/= marknote)

        -- Is there a mark in a subterm?
        isMarked = any (== marknote) . U.universe

        -- slicing strategy (1): drop until the mark,
        --                       drop the mark,
        --                       take until the next mark
        maySlice1 = takeWhile notMark . drop 1 . dropWhile notMark

        -- slicing strategy (2):
        -- keep only items which contains a mark as a subterm
        maySlice2 = filter isMarked

        -- Combine multiple strategies:
        --   takes the result of the first succeeding strategy
        comb fs xs = take 1 . filter (not . null) . map ($xs) $ fs
