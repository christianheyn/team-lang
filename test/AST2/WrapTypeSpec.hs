{-# LANGUAGE OverloadedStrings #-}

module AST2.WrapTypeSpec where

    import Test.Hspec
    import Text.RawString.QQ
    import qualified Data.ByteString.Lazy.Char8 as L
    import AST2
    import Data.Maybe

    spec :: Spec
    spec = do
        describe "___wrapType" $ do
            it "A B C xxx" $ do
                let actual = ___wrapType "A B C xxx"
                    expected = (
                            AST_VALUE [
                                AST_NODE {_astNodeType = AST_WrapType, _astValue = Nothing, _astChildren = AST_VALUE [
                                    AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "A", _astChildren = AST_VALUE []},
                                    AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "B", _astChildren = AST_VALUE []},
                                    AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "C", _astChildren = AST_VALUE []}
                                ]}
                            ]
                            ,"xxx"
                        )
                actual `shouldBe` expected

            it "A (B C) xxx" $ do
                let actual = ___wrapType "A (B C) xxx"
                    expected = (
                            AST_VALUE [
                                AST_NODE {_astNodeType = AST_WrapType, _astValue = Nothing, _astChildren = AST_VALUE [
                                    AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "A", _astChildren = AST_VALUE []},
                                    AST_NODE {_astNodeType = AST_WrapType, _astValue = Nothing, _astChildren = AST_VALUE [
                                        AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "B", _astChildren = AST_VALUE []},
                                        AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "C", _astChildren = AST_VALUE []}
                                    ]}
                                ]}
                            ]
                            ,"xxx"
                        )
                actual `shouldBe` expected
