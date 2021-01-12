{-# LANGUAGE OverloadedStrings #-}

module AST2.IfThenElseSpec where

    import Test.Hspec
    import Text.RawString.QQ
    import qualified Data.ByteString.Lazy.Char8 as L
    import AST2
    import Data.Maybe

    spec :: Spec
    spec = do
        describe "___ifThenElse" $ do
            it "if a then b else c xxx" $ do
                let actual = ___ifThenElse "if a then b else c xxx"
                    expected = (
                            AST_VALUE [
                                AST_NODE {_astNodeType = AST_IfStatement, _astValue = Nothing, _astChildren = AST_VALUE [
                                    AST_NODE {_astNodeType = AST_Symbol, _astValue = Just "a", _astChildren = AST_VALUE []},
                                    AST_NODE {_astNodeType = AST_Symbol, _astValue = Just "b", _astChildren = AST_VALUE []},
                                    AST_NODE {_astNodeType = AST_Symbol, _astValue = Just "c", _astChildren = AST_VALUE []}
                                ]}
                            ]
                            ,"xxx"
                        )
                actual `shouldBe` expected

            it "if a then b else if a then b else c xxx" $ do
                let actual = ___ifThenElse "if a then b else if a then b else c xxx"
                    expected = (
                            AST_VALUE [
                                AST_NODE {_astNodeType = AST_IfStatement, _astValue = Nothing, _astChildren = AST_VALUE [
                                    AST_NODE {_astNodeType = AST_Symbol, _astValue = Just "a", _astChildren = AST_VALUE []},
                                    AST_NODE {_astNodeType = AST_Symbol, _astValue = Just "b", _astChildren = AST_VALUE []},
                                    AST_NODE {_astNodeType = AST_IfStatement, _astValue = Nothing, _astChildren = AST_VALUE [
                                        AST_NODE {_astNodeType = AST_Symbol, _astValue = Just "a", _astChildren = AST_VALUE []},
                                        AST_NODE {_astNodeType = AST_Symbol, _astValue = Just "b", _astChildren = AST_VALUE []},
                                        AST_NODE {_astNodeType = AST_Symbol, _astValue = Just "c", _astChildren = AST_VALUE []}
                                    ]}
                                ]}
                            ]
                            ,"xxx"
                        )
                actual `shouldBe` expected

            it "if true then 3 else 5 xxx" $ do
                let actual = ___ifThenElse "if true then 3 else 5 xxx"
                    expected = (
                            AST_VALUE [
                                AST_NODE {_astNodeType = AST_IfStatement, _astValue = Nothing, _astChildren = AST_VALUE [
                                    AST_NODE {_astNodeType = AST_Bool, _astValue = Just "true", _astChildren = AST_VALUE []},
                                    AST_NODE {_astNodeType = AST_Number, _astValue = Nothing, _astChildren = AST_VALUE [
                                        AST_NODE {_astNodeType = AST_IntegerNumber, _astValue = Just "3", _astChildren = AST_VALUE []}
                                    ]},
                                    AST_NODE {_astNodeType = AST_Number, _astValue = Nothing, _astChildren = AST_VALUE [
                                        AST_NODE {_astNodeType = AST_IntegerNumber, _astValue = Just "5", _astChildren = AST_VALUE []}
                                    ]}
                                ]}
                            ]
                            ,"xxx"
                        )
                actual `shouldBe` expected
