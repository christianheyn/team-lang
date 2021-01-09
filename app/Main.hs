{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where
    import Text.RawString.QQ
    import Tokenizer
    import System.Environment
    import System.Directory
    import qualified Data.ByteString.Lazy.Char8 as L
    import AST2

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
        fileContent <- L.readFile $ cwd ++ "/" ++ filepath
        let ast = toAst fileContent
            valid = if isAstError ast || isAstKnowenError ast
                    then red ++ "--- Error ---" ++ noColor
                    else green ++ "--- Valid ---" ++ noColor
        -- print ast
        putStrLn valid

    main :: IO ()
    main = fileAst
