{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Syntax.TupleSpec where

    import Test.Hspec
    import Text.RawString.QQ
    import qualified Data.ByteString.Lazy.Char8 as L
    import Tokenizer
    import Syntax

    spec :: Spec
    spec = do
        describe "Syntax for tuples" $ do
            it "(pair 1 true)" $ do
                let actual = isTuple $ generateTokens "(pair 1 true)"
                    expected = ([],
                        [])
                actual `shouldBe` expected

            it "(pair 1+4i true)" $ do
                let actual = isTuple $ generateTokens "(pair 1+4i true)"
                    expected = ([],
                        [])
                actual `shouldBe` expected

            it "pair 1+4i true" $ do
                let actual = isTuple $ generateTokens "pair 1+4i true"
                    expected = ([],
                        [])
                actual `shouldBe` expected

            it "(triple 1 \"2\" true)" $ do
                let actual = isTuple $ generateTokens "(triple 1 \"2\" true)"
                    expected = ([],
                        [])
                actual `shouldBe` expected

            it "triple 1 \"2\" true" $ do
                let actual = isTuple $ generateTokens "triple 1 \"2\" true"
                    expected = ([],
                        [])
                actual `shouldBe` expected


            it "pair 1+4i triple false void 3+7i" $ do
                let actual = isTuple $ generateTokens "pair 1+4i triple false void 3+7i"
                    expected = ([],
                        [])
                actual `shouldBe` expected