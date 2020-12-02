{-# LANGUAGE OverloadedStrings #-}

module ASTSpec where

    import Test.Hspec
    import Text.RawString.QQ
    import qualified Data.ByteString.Lazy.Char8 as L
    import Tokenizer
    import Syntax

    spec :: Spec
    spec = do
        describe "Syntax" $ do
            it "Parameter: \"(a, b)\"" $ do
                let actual = isParamterList $ generateTokens "()"
                    expected = ([AstParameterList [] []],[])
                actual `shouldBe` expected

            it "List: \"[1 true false [1 a]]\"" $ do
                let actual = isList $ generateTokens "[1 [2 [3 true]]]"
                    expected = (
                        [AstList [] [
                            AstPrimitiv [Token {_TType = T_Number, _TValue = "1", _TIndex = 1}] [],
                            AstList [] [
                                AstPrimitiv [Token {_TType = T_Number, _TValue = "2", _TIndex = 4}] [],
                                AstList [] [
                                    AstPrimitiv [Token {_TType = T_Number, _TValue = "3", _TIndex = 7}] [],
                                    AstPrimitiv [Token {_TType = T_BooleanTrue, _TValue = "true", _TIndex = 9}] []
                                    ]
                                ]
                            ]
                        ],[])
                actual `shouldBe` expected
