{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where
    import Text.RawString.QQ
    import Tokenizer

    code = [r|

    }
    |]

    main :: IO ()
    main = print $ tokenize code
