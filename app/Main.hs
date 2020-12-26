{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where
    import Text.RawString.QQ
    import Tokenizer
    import System.Environment
    import System.Directory
    import qualified Data.ByteString.Lazy.Char8 as L
    import Tokenizer
    import Syntax

    noColor = "\x1b[0m"
    underline = "\x1b[4m"

    red = "\x1b[31m"
    magenta = "\x1b[35m"
    yellow = "\x1b[33m"
    green = "\x1b[32m"
    cyan = "\x1b[36m"
    grey = "\x1b[2m"
    whiteBg = "\x1b[7m"

    fileAst = do
        (filepath:_) <- getArgs
        cwd <- getCurrentDirectory
        x <- L.readFile $ cwd ++ "/" ++ filepath
        let tokens = generateTokens x
        print (last tokens)
        let ast = isTopLevel tokens
            valid = if length (snd ast) /= 0
                    then red ++ "--- Error ---" ++ noColor
                    else green ++ "--- Valid ---" ++ noColor
        putStrLn valid

    main :: IO ()
    main = fileAst
