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
            it "Parameter (a, b)" $ do
                let actual = isParamterList $ generateTokens "(a, b)"
                    expected = ([], [])
                actual `shouldBe` expected

            it "List [1 true false [1 a]]" $ do
                let actual = isList $ generateTokens "[1 [2 [3 true]]]"
                    expected = ([], [])
                actual `shouldBe` expected
