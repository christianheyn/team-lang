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

            it "-> T" $ do
                let actual = testResult $ symbol "-> T"
                    expected = (AST_Symbol, "->", "T")
                actual `shouldBe` expected

        describe "allSymbols" $ do
            it "lib.fn xxx" $ do
                let actual = allSymbols "lib.fn xxx"
                    expected =  (
                            AST_VALUE [
                                AST_NODE {_astNodeType = AST_ImportedSymbol, _astValue = Nothing, _astChildren = AST_VALUE [
                                    AST_NODE {_astNodeType = AST_Symbol, _astValue = Just "lib", _astChildren = AST_VALUE []},
                                    AST_NODE {_astNodeType = AST_Symbol, _astValue = Just "fn", _astChildren = AST_VALUE []}
                                ]}
                            ]
                            ,"xxx"
                        )
                actual `shouldBe` expected

        describe "typeSymbol" $ do
            it "Hello xxx" $ do
                let actual = testResult $ typeSymbol "Hello xxx"
                    expected = (AST_TypeSymbol, "Hello", "xxx")
                actual `shouldBe` expected

            it "NUMBER xxx" $ do
                let actual = testResult $ typeSymbol "NUMBER xxx"
                    expected = (AST_TypeSymbol, "NUMBER", "xxx")
                actual `shouldBe` expected

            it "T ->" $ do
                let actual = testResult $ typeSymbol "T ->"
                    expected = (AST_TypeSymbol, "T", "->")
                actual `shouldBe` expected

            it "T->" $ do
                let actual = testResult $ typeSymbol "T->"
                    expected = (AST_TypeSymbol, "T", "->")
                actual `shouldBe` expected

        describe "allTypeSymbols" $ do
            it "lib.TYPE xxx" $ do
                let actual = allTypeSymbols "lib.TYPE xxx"
                    expected = (
                            AST_VALUE [
                                AST_NODE {_astNodeType = AST_ImportedTypeSymbol, _astValue = Nothing, _astChildren = AST_VALUE [
                                    AST_NODE {_astNodeType = AST_Symbol, _astValue = Just "lib", _astChildren = AST_VALUE []},
                                    AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "TYPE", _astChildren = AST_VALUE []}
                                ]}
                            ]
                            ,"xxx"
                        )
                actual `shouldBe` expected

            it "( lib.TYPE ) xxx" $ do
                let actual = allTypeSymbols "( lib.TYPE ) xxx"
                    expected = (
                            AST_VALUE [
                                AST_NODE {_astNodeType = AST_ImportedTypeSymbol, _astValue = Nothing, _astChildren = AST_VALUE [
                                    AST_NODE {_astNodeType = AST_Symbol, _astValue = Just "lib", _astChildren = AST_VALUE []},
                                    AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "TYPE", _astChildren = AST_VALUE []}
                                ]}
                            ]
                            ,"xxx"
                        )
                actual `shouldBe` expected

            it "(TYPE) xxx" $ do
                let actual = allTypeSymbols "(TYPE) xxx"
                    expected = (
                            AST_VALUE [
                                AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "TYPE", _astChildren = AST_VALUE []}
                            ]
                            ,"xxx"
                        )
                actual `shouldBe` expected