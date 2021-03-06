module Language.LaTeX
  (
   -- * Types
   Document
  ,LineNumber
  ,CharNumber
  ,Loc
  ,Note
  ,Named
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
  ,Encoding
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
  ,printLatexDocument
  ,ViewOpts(..)
  ,quickView
  ,myViewOpts
  ,testViewOpts
  ,showLaTeX
  ,runLatexM
  )
where
import Data.List (intercalate)
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
import System.IO (withBinaryFile, IOMode(WriteMode), hPutStr)
import Codec.Binary.UTF8.String (encode)

data ViewOpts = ViewOpts { basedir   :: FilePath
                         , pdflatex  :: String
                         , pdfviewer :: String
                         , inputdirs :: [FilePath]
                         , showoutput :: Bool }

myViewOpts, testViewOpts :: ViewOpts
myViewOpts = ViewOpts { basedir   = ""
                      , pdflatex  = "texi2pdf"
                      , pdfviewer = "open"
                      , inputdirs = []
                      , showoutput = True }

testViewOpts = myViewOpts { basedir = "tests" }

writeFileUTF8 :: FilePath -> String -> IO ()
writeFileUTF8 n s = withBinaryFile n WriteMode (\hdl -> hPutStr hdl txt)
   where txt = map (toEnum . fromEnum) (encode s)

quickView :: ViewOpts -> FilePath -> LatexM Document -> IO ()
quickView vo basename doc =
     do createDirectoryIfMissing False (basedir vo)
        when (showoutput vo) $ putStrLn s
        writeFileUTF8 (basedir vo </> tex) s
        exitWith =<< system cmd
  -- where s = encodeString . either error id $ showLaTeX doc
  where s = either error id $ showLaTeX doc
        pdf = basename <.> "pdf"
        tex = basename <.> "tex"
        inputdirsStr = intercalate ":" $ inputdirs vo
        texinputs = "TEXINPUTS=" ++ inputdirsStr
        bstinputs = "BSTINPUTS=" ++ inputdirsStr
        cmd = unwords ["cd", basedir vo, "&&"
                      , texinputs, bstinputs, pdflatex vo, tex, "&&"
                      , pdfviewer vo, pdf]

printLatexDocument :: LatexM Document -> IO ()
printLatexDocument = putStr . either error id . showLaTeX
