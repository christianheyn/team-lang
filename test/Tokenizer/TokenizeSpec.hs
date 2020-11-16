{-# LANGUAGE OverloadedStrings #-}

module Tokenizer.TokenizeSpec where

    import Test.Hspec
    import Text.RawString.QQ
    import qualified Data.ByteString.Lazy.Char8 as L
    import Tokenizer

    -- _TEST_T is just a shortcut to write a token quicker by hand
    _TEST_T (t, v, i) = Token { _TType = t, _TValue = v,  _TIndex = i }

    testTokenize code tokenList = do
        it code $ do
            let actual = generateTokens $ L.pack code
                expected = map _TEST_T tokenList
            actual `shouldBe` expected

    spec :: Spec
    spec = do
        describe "tokenize" $ do

            it "not" $ do
                let actual = tokenize "feature {t () ()}"
                    expected = ["feature"," ","{","t"," ","(",")"," ","(",")","}"]
                actual `shouldBe` expected

        describe "generateTokens" $ do

            testTokenize "feature {t () ()}" [
                  (T_FlagKeyword, "feature", 0)
                , (T_OpenCurlyBracket, "{", 2)
                , (T_Symbol, "t", 3)
                , (T_OpenRoundBracket, "(", 5)
                , (T_ClosingRoundBracket, ")", 6)
                , (T_OpenRoundBracket, "(", 8)
                , (T_ClosingRoundBracket, ")", 9)
                , (T_ClosingCurlyBracket, "}", 10)
                ]

