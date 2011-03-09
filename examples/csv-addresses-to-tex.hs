{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
import Language.LaTeX
import qualified Language.LaTeX.Builder as B
import qualified Language.LaTeX.Builder.Babel as B
import qualified Language.LaTeX.Length as L
import qualified Language.LaTeX.Builder.Internal as BI
-- import qualified Language.LaTeX.Builder.Math as M
-- import qualified Language.LaTeX.Builder.Graphics as G
-- import qualified Language.LaTeX.Builder.Rotating as R
import qualified Language.LaTeX.Builder.Color as C

-- import Data.Ratio ((%))
import Data.Char
import Data.List.Split
import Data.List
import Data.String
import Data.Maybe
import Data.Foldable (foldMap)
import Data.Monoid.Unicode
import Text.CSV (parseCSV, CSV, Record, Field)
import System.Environment
import qualified Safe
import System.IO (stdout,hSetEncoding,utf8)

mainArgs [] = do hSetEncoding stdout utf8
                 putStr . fromAddrs =<< getContents
  where fromAddrs = either error id           . showLaTeX    -- show
                                              . body         -- LaTeX it
                  . fromMaybe []              . addrFromCSV  -- load records
                  . either (error . show) id  . parseCSV "-" -- parse CSV
mainArgs _ = error "Usage: csv-addresses-to-pdf"

main = mainArgs =<< getArgs

-- paraNoindent = B.para . (B.noindent ⊕)

preamb = B.useBabel B.francais []
       ⊕ BI.usepackage ["utf8"] (BI.pkgName "inputenc")

data Addr = Addr { name    :: String
                 , street  :: String
                 , zipcode :: String
                 , city    :: String
                 , country :: Maybe String
                 }

texLines = mconcat . intersperse B.newline . filter (/= ø)

texAddr :: Addr -> ParItem
texAddr Addr{..} =
  B.para (texLines
   [ck "Nom" name
   ,ck "Adresse" street
   ,ck "Code Postal" zipcode ⊕ B.space ⊕ ck "Ville" city
   ,foldMap fromString country])
  ⊕ B.vspace (L.em 0.6)
  where ck nam x
         | all isSpace x = C.textcolor C.red (fromString nam)
         | otherwise     = fromString x


texAddrs :: [Addr] -> ParItem
texAddrs = foldMap texAddrPage . chunk 20 -- . prolongateByMod def 2
  where
    -- def = Addr "" "" "" "" Nothing
    cols = (2 :: Int)
    wi = L.linewidth / fromIntegral cols
    texAddrPage addrs =
      B.tabular (replicate cols B.l) (
          intersperse (B.cells (replicate cols "")) .
          map (B.cells . texAddrRow) $ chunk cols addrs)
      ⊕
      B.newpage

    texAddrRow a =
      take cols (map (B.minipage wi . texAddr) a
                 ++ repeat ø)

{-
-- think more about this
foldl'Append :: (a -> b -> a) -> a -> (a -> [b]) -> [b] -> [b]
foldl'Append _ z k []     = k z
foldl'Append f z k (x:xs) = z `seq` (x : foldl'Append f (f z x) k xs)

prolongateByMod :: a -> Int -> [a] -> [a]
prolongateByMod x modu = foldl'Append (const . succ) 0 f
  where f len = replicate (len `mod` modu) x
  -}

type HeaderRecord = Record
csvSelector :: HeaderRecord -> Field -> Maybe (Record -> Field)
csvSelector header field = flip (Safe.atDef ø) `fmap` findIndex (==field) header

addrFromCSV :: CSV -> Maybe [Addr]
addrFromCSV [] = Nothing
addrFromCSV (header:csv) = do addrLoader <- addrLoaderFromCSV header
                              return $ map addrLoader csv

addrLoaderFromCSV :: HeaderRecord -> Maybe (Record -> Addr)
addrLoaderFromCSV header = 
     do name      <- sel "Nom"
        firstname <- sel "Prénom"
        pre       <- sel "Complément d'adresse"
        street    <- sel "Adresse"
        zipcode   <- sel "Code Postal"
        city      <- sel "Ville"
        let build r = Addr { name    = name r ++ " " ++ firstname r
                           , street  = (if null prer then "" else prer ++ " ") ++ street r
                           , zipcode = zipcode r
                           , city    = city r
                           , country = Nothing }
              where prer = pre r
        return build
  where sel = csvSelector header

body addrs = B.document dc preamb (B.pagestyle "empty" ⊕ texAddrs addrs)
  where dc = B.article (Just (L.pt 12)) (Just B.a4paper) []
