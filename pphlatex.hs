import System.Environment
import System.Cmd
import System.Exit

whileSuccess :: [IO ExitCode] -> IO ExitCode
whileSuccess [] = return ExitSuccess
whileSuccess (x:xs) = do code <- x
                         case code of
                           ExitSuccess -> whileSuccess xs
                           _ -> return code

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> exitWith =<< system "mempty | frquotes"
    [in1,inp,outp] -> exitWith =<< whileSuccess [rawSystem "frquotes" [in1, inp, tmpp]
                                                ,rawSystem "mempty" [in1, tmpp, outp]]
                   where tmpp = outp ++ ".pphlatex"
    _  -> fail "Usage: pphlatex [orig input output]"
