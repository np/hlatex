module Language.LaTeX
  (module Language.LaTeX.Types
  ,module Language.LaTeX.Builder.MonoidUtils
  ,(!<), (<!), (!<!), tell, Writer
  ,Monoid(..)
  ,ViewOpts(..)
  ,quickView
  ,myViewOpts
  ,testViewOpts
  )
where
import Data.Monoid (Monoid(..))
import Language.LaTeX.Types
import Language.LaTeX.Printer (showLaTeX)
import Language.LaTeX.Builder.MonoidUtils
import Language.LaTeX.Builder ((!<), (<!), (!<!))
import Control.Monad.Writer (Writer, tell)
import System.Cmd (system)
import System.FilePath
import System.Exit

data ViewOpts = ViewOpts { basedir   :: FilePath
                         , pdflatex  :: String
                         , pdfviewer :: String }

myViewOpts, testViewOpts :: ViewOpts
myViewOpts = ViewOpts { basedir   = ""
                      , pdflatex  = "texi2pdf"
                      , pdfviewer = "open" }

testViewOpts = myViewOpts { basedir = "tests" }

quickView :: ViewOpts -> FilePath -> LatexM Root -> IO ()
quickView vo basename root =
     do putStrLn s
        writeFile (basedir vo </> ltx) s
        exitWith =<< system cmd
  where s = either error id $ showLaTeX root
        pdf = basename <.> "pdf"
        ltx = basename <.> "ltx"
        cmd = unwords ["cd", basedir vo, "&&", pdflatex vo, ltx, "&&", pdfviewer vo, pdf]
