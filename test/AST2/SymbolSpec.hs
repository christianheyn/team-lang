{-# LANGUAGE OverloadedStrings #-}

module AST2.SymbolSpec where

    import Test.Hspec
    import Text.RawString.QQ
    import qualified Data.ByteString.Lazy.Char8 as L
    import AST2
    import Data.Maybe

    testResult :: AstResult -> (AST_NODE_TYPE, L.ByteString, L.ByteString)
    testResult (ast, rest) = (
            _astNodeType fistNode
            , (fromJust . _astValue) fistNode
            , rest)
        where fistNode = (head . fromAST) ast

    spec :: Spec
    spec = do
        describe "symbol" $ do
            it "hello xxx" $ do
                let actual = testResult $ symbol "hello xxx"
                    expected = (AST_Symbol, "hello", "xxx")
                actual `shouldBe` expected

            it "<html> xxx" $ do
                let actual = testResult $ symbol "<html> xxx"
                    expected = (AST_Symbol, "<html>", "xxx")
                actual `shouldBe` expected

            it "<++?~-> xxx" $ do
                let actual = testResult $ symbol "<++?~-> xxx"
                    expected = (AST_Symbol, "<++?~->", "xxx")
                actual `shouldBe` expected

            it "🐒 xxx" $ do
                let actual = testResult $ symbol "🐒 xxx"
                    expected = (AST_Symbol, "🐒", "xxx")
                actual `shouldBe` expected

            it "a234 xxx" $ do
                let actual = testResult $ symbol "a234 xxx"
                    expected = (AST_Symbol, "a234", "xxx")
                actual `shouldBe` expected

            it "a234(xxx" $ do
                let actual = testResult $ symbol "a234(xxx"
                    expected = (AST_Symbol, "a234", "(xxx")
                actual `shouldBe` expected

            it "__&&$$!!(xxx" $ do
                let actual = testResult $ symbol "__&&$$!!(xxx"
                    expected = (AST_Symbol, "__&&$$!!", "(xxx")
                actual `shouldBe` expected

            it "number-complex (xxx" $ do
                let actual = testResult $ symbol "number-complex (xxx"
                    expected = (AST_Symbol, "number-complex", "(xxx")
                actual `shouldBe` expected

