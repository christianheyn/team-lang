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
                  (T_FlagFeature, "feature", 0)
                , (T_OpenCurlyBracket, "{", 2)
                , (T_Symbol, "t", 3)
                , (T_OpenRoundBracket, "(", 5)
                , (T_ClosingRoundBracket, ")", 6)
                , (T_OpenRoundBracket, "(", 8)
                , (T_ClosingRoundBracket, ")", 9)
                , (T_ClosingCurlyBracket, "}", 10)
                ]

            testTokenize "[1 2 3]" [
                  (T_OpenSquareBracket, "[", 0)
                , (T_Number, "1", 1)
                , (T_Number, "2", 3)
                , (T_Number, "3", 5)
                , (T_ClosingSquareBracket, "]", 6)
                ]
            testTokenize "[1, 2, 3]" [
                  (T_OpenSquareBracket, "[", 0)
                , (T_Number, "1", 1)
                , (T_Number, "2", 4)
                , (T_Number, "3", 7)
                , (T_ClosingSquareBracket, "]", 8)
                ]
            testTokenize "[1, 2, 3,]" [
                  (T_OpenSquareBracket, "[", 0)
                , (T_Number, "1", 1)
                , (T_Number, "2", 4)
                , (T_Number, "3", 7)
                , (T_ClosingSquareBracket, "]", 9)
                ]

            testTokenize "{add (x y) (+ x y)}" [
                  (T_OpenCurlyBracket, "{", 0)
                , (T_Symbol, "add", 1)
                , (T_OpenRoundBracket, "(", 3)
                , (T_Symbol, "x", 4)
                , (T_Symbol, "y", 6)
                , (T_ClosingRoundBracket, ")", 7)
                , (T_OpenRoundBracket, "(", 9)
                , (T_Symbol, "+", 10)
                , (T_Symbol, "x", 12)
                , (T_Symbol, "y", 14)
                , (T_ClosingRoundBracket, ")", 15)
                , (T_ClosingCurlyBracket, "}", 16)
                ]

            testTokenize "(+ 2+3i 3)" [
                  (T_OpenRoundBracket, "(", 0)
                , (T_Symbol, "+", 1)
                , (T_ComplexNumber, "2+3i", 3)
                , (T_Number, "3", 5)
                , (T_ClosingRoundBracket, ")", 6)
                ]

            testTokenize "(let (a 1/3) ())" [
                  (T_OpenRoundBracket, "(", 0)
                , (T_Let, "let", 1)
                , (T_OpenRoundBracket, "(", 3)
                , (T_Symbol, "a", 4)
                , (T_Number, "1/3", 6)
                , (T_ClosingRoundBracket, ")", 7)
                , (T_OpenRoundBracket, "(", 9)
                , (T_ClosingRoundBracket, ")", 10)
                , (T_ClosingRoundBracket, ")", 11)
                ]

            testTokenize "(if (< 2 3) (print \"yes\"))" [
                  (T_OpenRoundBracket, "(", 0)
                , (T_If, "if", 1)
                , (T_OpenRoundBracket, "(", 3)
                , (T_Symbol, "<", 4)
                , (T_Number, "2", 6)
                , (T_Number, "3", 8)
                , (T_ClosingRoundBracket, ")", 9)
                , (T_OpenRoundBracket, "(", 11)
                , (T_Symbol, "print", 12)
                , (T_String, "\"yes\"", 14)
                , (T_ClosingRoundBracket, ")", 15)
                , (T_ClosingRoundBracket, ")", 16)
                ]

            testTokenize "(type A [(& String Number)])" [
                  (T_OpenRoundBracket, "(", 0)
                , (T_TypeKeyword, "type", 1)
                , (T_Type, "A", 3)
                , (T_OpenSquareBracket, "[", 5)
                , (T_OpenRoundBracket, "(", 6)
                , (T_Symbol, "&", 7)
                , (T_Type, "String", 9)
                , (T_Type, "Number", 11)
                , (T_ClosingRoundBracket, ")", 12)
                , (T_ClosingSquareBracket, "]", 13)
                , (T_ClosingRoundBracket, ")", 14)
                ]

            testTokenize "(types (A [String]) (B [Number]))" [
                (T_OpenRoundBracket, "(", 0),
                (T_Symbol, "types", 1),
                (T_OpenRoundBracket, "(", 3),
                (T_Type, "A", 4),
                (T_OpenSquareBracket, "[", 6),
                (T_Type, "String", 7),
                (T_ClosingSquareBracket, "]", 8),
                (T_ClosingRoundBracket, ")", 9),
                (T_OpenRoundBracket, "(", 11),
                (T_Type, "B", 12),
                (T_OpenSquareBracket, "[", 14),
                (T_Type, "Number", 15),
                (T_ClosingSquareBracket, "]", 16),
                (T_ClosingRoundBracket, ")", 17),
                (T_ClosingRoundBracket, ")", 18)]

            testTokenize "(var t Boolean true)" [
                  (T_OpenRoundBracket, "(", 0)
                , (T_Var, "var", 1)
                , (T_Symbol, "t", 3)
                , (T_Type, "Boolean", 5)
                , (T_BooleanTrue, "true", 7)
                , (T_ClosingRoundBracket, ")", 8)
                ]

            testTokenize "a = (/ (* 3 pi) e)" [
                  (T_Symbol, "a", 0)
                , (T_EqualSign, "=", 2)
                , (T_OpenRoundBracket, "(", 4)
                , (T_Symbol, "/", 5)
                , (T_OpenRoundBracket, "(", 7)
                , (T_Symbol, "*", 8)
                , (T_Number, "3", 10)
                , (T_Number, "pi", 12)
                , (T_ClosingRoundBracket, ")", 13)
                , (T_Number, "e", 15)
                , (T_ClosingRoundBracket, ")", 16)
                ]

            testTokenize "T|U|V"  [
                  (T_Type, "T", 0)
                , (T_Symbol, "|", 1)
                , (T_Type, "U", 2)
                , (T_Symbol, "|", 3)
                , (T_Type, "V", 4)
                ]
