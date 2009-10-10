import System.Environment

-- substitutes UTF8 empty set (∅) for (mempty)
-- this share most code with frquotes

interact' :: (String -> String) -> IO ()
interact' f = do
  args <- getArgs
  case args of
    [] -> interact f
    [in1,inp,outp] -> writeFile outp =<< ((("{-# LINE 2 \""++in1++"\" #-}\n")++) . f)
                                `fmap` readFile inp
    _  -> fail "Usage: mempty [orig input output]"

main :: IO ()
main = interact' h
        -- All these functions follow the same style,
        -- the first argument 'k' is the continuation, i.e. what to do next.
        -- Each function search for a different closing token, if the closing
        -- token is found the continuation is called with the remaining char
        -- stream, if some opening token is found the continuation is stacked
        -- as the continuation of a new function call that have to find this
        -- new token first.

        -- haskell context
        -- the h function don't needs a continuation parameter
  where h ""                   = ""
        h ('{':'-':xs)         = "{-" ++ c (("-}"++) . h) xs
        h ('"':xs)             = '"' : s (('"':) . h) xs
        h ('\'':xs)            = '\'' : a h xs
        h ('[':'$':xs)         = '[' : '$' : startq h xs
        h ('\226':'\136':'\133':xs) = "(mempty)" ++ h xs
        h ('\226':'\138':'\149':xs) = "`mappend`" ++ h xs
        h (x:xs)               = x : h xs

        -- haskell (nested) comments
        c _ ""                 = error "unterminated haskell comment (expecting `-}')"
        c k ('{':'-':xs)       = "{-" ++ c (("-}"++) . c k) xs
        c k ('-':'}':xs)       = k xs
        c k (x:xs)             = x : c k xs

        -- haskell strings literal
        s _ ""                 = error "unterminated haskell string (expecting `\"')"
        s k ('\\':x:xs)        = '\\' : x : s k xs
        s k ('"':xs)           = k xs
        s k (x:xs)             = x : s k xs

        -- haskell char literal (a bit lenient)
        a _ ""                 = error "unterminated haskell character (expecting `'')"
        a k ('\\':x:xs)        = '\\' : x : a k xs
        a k (x:'\'':xs)
                   | x /= '\'' = x : '\'' : k xs
        a k xs                 = k xs

        -- haskell quasi-quotation
        -- is there nested QQ?
        q _ ""                 = error "unterminated haskell quasi-quotation (expecting `|]')"
        q k ('|':']':xs)       = '|' : ']' : k xs
        q k (x:xs)             = x : q k xs
        startq k xs = ys ++ '|' : q k zs'
          where (ys,zs) = break (=='|') xs
                zs' | null zs   = error "unrecognized haskell quasi-quotation (expecting `|`)"
                    | otherwise = drop 1 zs
