{-# LANGUAGE OverloadedStrings #-}

module Tokenizer.RegexSpec where

    import Test.Hspec
    import Text.RawString.QQ
    import qualified Data.ByteString.Lazy.Char8 as L
    import Tokenizer

    spec :: Spec
    spec = do
        describe "Token Regex" $ do

            describe "_isId" $ do
                it "true when one letter" $ do
                    let actual = _isId "a"
                        expected = True
                    actual `shouldBe` expected

                    let actual = _isId "_23"
                        expected = True
                    actual `shouldBe` expected

                it "false when space" $ do
                    let actual = _isId "\naaa"
                        expected = False
                    actual `shouldBe` expected

            describe "_isType" $ do
                it "true when one letter" $ do
                    let actual = _isType "A"
                        expected = True
                    actual `shouldBe` expected

                    let actual = _isType "Z"
                        expected = True
                    actual `shouldBe` expected

                it "true when first letter uppercase" $ do
                    let actual = _isType "Abc"
                        expected = True
                    actual `shouldBe` expected

                    let actual = _isType "Z_a"
                        expected = True
                    actual `shouldBe` expected

                    let actual = _isType "Z012_Ax"
                        expected = True
                    actual `shouldBe` expected

                    let actual = _isType "Z___"
                        expected = True
                    actual `shouldBe` expected

            describe "_isUnresolvedNumber" $ do
                it "true when ends with ." $ do
                    let actual = _isUnresolvedNumber "0."
                        expected = True
                    actual `shouldBe` expected

                    let actual = _isUnresolvedNumber "23."
                        expected = True
                    actual `shouldBe` expected

                it "false when starting with 0" $ do
                    let actual = _isUnresolvedNumber "00."
                        expected = False
                    actual `shouldBe` expected

                    let actual = _isUnresolvedNumber "023."
                        expected = False
                    actual `shouldBe` expected



            describe "_isNumber" $ do
                it "true for natural numbers" $ do
                    let actual = _isNumber "0"
                        expected = True
                    actual `shouldBe` expected

                    let actual = _isNumber "23"
                        expected = True
                    actual `shouldBe` expected

                    let actual = _isNumber "2321376197319827538125347126543615234"
                        expected = True
                    actual `shouldBe` expected

                it "true for float numbers" $ do
                    let actual = _isNumber "0.0"
                        expected = True
                    actual `shouldBe` expected

                    let actual = _isNumber "0.2"
                        expected = True
                    actual `shouldBe` expected

                    let actual = _isNumber "121231230.89712098376912870"
                        expected = True
                    actual `shouldBe` expected

                it "true for negative numbers" $ do
                    let actual = _isNumber "-0"
                        expected = True
                    actual `shouldBe` expected

                    let actual = _isNumber "-0.0"
                        expected = True
                    actual `shouldBe` expected

                    let actual = _isNumber "-3"
                        expected = True
                    actual `shouldBe` expected

                    let actual = _isNumber "-0.2"
                        expected = True
                    actual `shouldBe` expected

                    let actual = _isNumber "-121231230.89712098376912870"
                        expected = True
                    actual `shouldBe` expected

                it "false when starting with 0" $ do
                    let actual = _isNumber "00.0"
                        expected = False
                    actual `shouldBe` expected

                    let actual = _isNumber "0123"
                        expected = False
                    actual `shouldBe` expected

                it "false when more then one ." $ do
                    let actual = _isNumber "0..0"
                        expected = False
                    actual `shouldBe` expected

                    let actual = _isNumber "0.12.3"
                        expected = False
                    actual `shouldBe` expected

                it "false when no number after ." $ do
                    let actual = _isNumber "0."
                        expected = False
                    actual `shouldBe` expected

                    let actual = _isNumber "123."
                        expected = False
                    actual `shouldBe` expected

            describe "_endingChars" $ do
                it "test\"" $ do
                    let actual = _endingChars '"' "test\""
                        expected = 1
                    actual `shouldBe` expected

                it "test\\\"" $ do
                    let actual = _endingChars '"' "test\\\""
                        expected = 0
                    actual `shouldBe` expected

            describe "_isUnresolvedString" $ do
                it "\"test" $ do
                    let actual = _isUnresolvedString "\"test"
                        expected = True
                    actual `shouldBe` expected

                it "\"test\\\"" $ do
                    let actual = _isUnresolvedString "\"test\\\""
                        expected = True
                    actual `shouldBe` expected

                it "\"test\\\"\"" $ do
                    let actual = _isUnresolvedString "\"test\\\"\""
                        expected = False
                    actual `shouldBe` expected

                it "\"test\\\"\"a" $ do
                    let actual = _isUnresolvedString "\"test\\\"\"a"
                        expected = False
                    actual `shouldBe` expected

            describe "_isString" $ do
                it "test" $ do
                    let actual = _isString "test"
                        expected = False
                    actual `shouldBe` expected

                it "test\"" $ do
                    let actual = _isString "test\""
                        expected = False
                    actual `shouldBe` expected

                it "\"test\\\"" $ do
                    let actual = _isString "\"test\\\""
                        expected = False
                    actual `shouldBe` expected

                it "\"\"" $ do
                    let actual = _isString "\"\""
                        expected = True
                    actual `shouldBe` expected

                it "\"test\"" $ do
                    let actual = _isString "\"test\""
                        expected = True
                    actual `shouldBe` expected

                it "\"test\"a" $ do
                    let actual = _isString "\"test\"a"
                        expected = False
                    actual `shouldBe` expected

                it "\"test\ntest\"" $ do
                    let actual = _isString "\"test\ntest\""
                        expected = True
                    actual `shouldBe` expected

                it "\"test\ntest\"a" $ do
                    let actual = _isString "\"test\ntest\"a"
                        expected = False
                    actual `shouldBe` expected


            describe "_isOperator" $ do
                it "=>" $ do
                    let actual = _isOperator "=>"
                        expected = True
                    actual `shouldBe` expected

                it "->" $ do
                    let actual = _isOperator "->"
                        expected = True
                    actual `shouldBe` expected

                it "::" $ do
                    let actual = _isOperator "::"
                        expected = True
                    actual `shouldBe` expected

                it "?" $ do
                    let actual = _isOperator "?"
                        expected = True
                    actual `shouldBe` expected

                it "!" $ do
                    let actual = _isOperator "!"
                        expected = True
                    actual `shouldBe` expected

                it "-" $ do
                    let actual = _isOperator "-"
                        expected = True
                    actual `shouldBe` expected

                it "+" $ do
                    let actual = _isOperator "+"
                        expected = True
                    actual `shouldBe` expected

                it "*" $ do
                    let actual = _isOperator "*"
                        expected = True
                    actual `shouldBe` expected

                it "|" $ do
                    let actual = _isOperator "|"
                        expected = True
                    actual `shouldBe` expected

                it "/" $ do
                    let actual = _isOperator "/"
                        expected = True
                    actual `shouldBe` expected

                it "not #" $ do
                    let actual = _isOperator "#"
                        expected = False
                    actual `shouldBe` expected



            describe "_isComment" $ do
                it "# " $ do
                    let actual = _isComment "# "
                        expected = True
                    actual `shouldBe` expected

                it "# comment" $ do
                    let actual = _isComment "# comment test"
                        expected = True
                    actual `shouldBe` expected

                it "# comment # in comment" $ do
                    let actual = _isComment "# comment # in comment"
                        expected = True
                    actual `shouldBe` expected

            describe "_isSpace" $ do
                it "___" $ do
                    let actual = _isSpace "   "
                        expected = True
                    actual `shouldBe` expected

            describe "_isNewline" $ do
                it "newline newline" $ do
                    let actual = _isNewline "\n"
                        expected = True
                    actual `shouldBe` expected


