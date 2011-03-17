module Language.LaTeX
  (
   -- * Types
   Document
  ,LineNumber
  ,CharNumber
  ,Loc
  ,Note
  ,DocumentClassKind
  ,DocumentClss
  ,Star
  ,Coord
  ,Percentage
  ,TexUnit
  ,LatexLength
  ,RowSpec
  ,LocSpec
  ,Pos
  ,LatexPaperSize
  ,Row
  ,PackageName
  ,Key
  ,SaveBin
  ,LatexState
  ,TexDecl
  ,LatexItem
  ,ParItem
  ,MathDecl
  ,AnyItem
  ,MathItem
  ,ListItem
  ,PreambleItem
  ,DocumentClass
  ,LatexM
  -- * Internal types
  ,Arg
  ,MathDcl
  ,AnyItm
  ,PreambleItm
  ,TexDcl
  ,LatexItm
  ,ParItm
  ,MathItm
  ,ListItm
  -- * Writer type aliases
  ,TexDeclW
  ,LatexItemW
  ,ParItemW
  ,MathDeclW
  ,MathItemW
  ,PreambleItemW
  -- * Utils
  ,module Language.LaTeX.Builder.MonoidUtils
  ,(!$), ($?), (!$?), tell, Writer
  ,(★)
  ,Monoid(..)
  ,ViewOpts(..)
  ,quickView
  ,myViewOpts
  ,testViewOpts
  ,showLaTeX
  )
where
import Data.Monoid (Monoid(..))
import Language.LaTeX.Types
import Language.LaTeX.Printer (showLaTeX)
import Language.LaTeX.Builder.MonoidUtils
import Language.LaTeX.Builder ((!$), ($?), (!$?), (★))
import Control.Monad (when)
import Control.Monad.Writer (Writer, tell)
import System.Cmd (system)
import System.FilePath
import System.Directory (createDirectoryIfMissing)
import System.Exit
import qualified System.IO.UTF8 as U
-- import Codec.Binary.UTF8.String (encodeString)

data ViewOpts = ViewOpts { basedir   :: FilePath
                         , pdflatex  :: String
                         , pdfviewer :: String
                         , showoutput :: Bool }

myViewOpts, testViewOpts :: ViewOpts
myViewOpts = ViewOpts { basedir   = ""
                      , pdflatex  = "texi2pdf"
                      , pdfviewer = "open"
                      , showoutput = True }

testViewOpts = myViewOpts { basedir = "tests" }

{-
-- | The computation 'writeBinaryFile' @file str@ function writes the string @str@,
-- to the file @file@.
writeBinaryFile :: FilePath -> String -> IO ()
writeBinaryFile f txt = withBinaryFile f WriteMode (\ hdl -> hPutStr hdl txt)
-}

quickView :: ViewOpts -> FilePath -> LatexM Document -> IO ()
quickView vo basename doc =
     do createDirectoryIfMissing False (basedir vo)
        when (showoutput vo) $ putStrLn s
        -- writeBinaryFile (basedir vo </> ltx) s
        U.writeFile (basedir vo </> ltx) s
        exitWith =<< system cmd
  -- where s = encodeString . either error id $ showLaTeX doc
  where s = either error id $ showLaTeX doc
        pdf = basename <.> "pdf"
        ltx = basename <.> "ltx"
        cmd = unwords ["cd", basedir vo, "&&", pdflatex vo, ltx, "&&", pdfviewer vo, pdf]
