{-# LANGUAGE OverloadedStrings #-}

module AST2.KeywordSpec where

    import Test.Hspec
    import Text.RawString.QQ
    import qualified Data.ByteString.Lazy.Char8 as L
    import AST2
    import Data.Maybe

    spec :: Spec
    spec = do
        describe "keyword" $ do
            it "if xxx" $ do
                let actual = keywordIf "if xxx"
                    expected = (
                            AST_VALUE []
                            , "xxx"
                        )
                actual `shouldBe` expected

            it "if(xxx" $ do
                let actual = keywordIf "if(xxx"
                    expected = (
                            AST_VALUE []
                            , "(xxx"
                        )
                actual `shouldBe` expected

            it "then xxx" $ do
                let actual = keywordThen "then xxx"
                    expected = (
                            AST_VALUE []
                            , "xxx"
                        )
                actual `shouldBe` expected

            it "then(xxx" $ do
                let actual = keywordThen "then(xxx"
                    expected = (
                            AST_VALUE []
                            , "(xxx"
                        )
                actual `shouldBe` expected

            it "else xxx" $ do
                let actual = keywordElse "else xxx"
                    expected = (
                            AST_VALUE []
                            , "xxx"
                        )
                actual `shouldBe` expected

            it "else(xxx" $ do
                let actual = keywordElse "else(xxx"
                    expected = (
                            AST_VALUE []
                            , "(xxx"
                        )
                actual `shouldBe` expected

            it "maybe XXX" $ do
                let actual = keywordMaybe "maybe XXX"
                    expected = (
                            AST_VALUE []
                            , "XXX"
                        )
                actual `shouldBe` expected

            it "either XXX" $ do
                let actual = keywordEither "either XXX"
                    expected = (
                            AST_VALUE []
                            , "XXX"
                        )
                actual `shouldBe` expected
