{-# LANGUAGE OverloadedStrings #-}

module AST2.FunctionSpec where

    import Test.Hspec
    import Text.RawString.QQ
    import qualified Data.ByteString.Lazy.Char8 as L
    import AST2

    spec :: Spec
    spec = do
        describe "AST2 functions" $ do
            describe "functionDef" $ do
                it "{f () 3} xxx" $ do
                    let actual = functionDef "{f () 3} xxx"
                        expected = (
                                AST_VALUE [
                                    AST_NODE {_astNodeType = AST_Function, _astValue = Nothing, _astChildren = AST_VALUE [
                                        AST_NODE {_astNodeType = AST_Symbol, _astValue = Just "f", _astChildren = AST_VALUE []},
                                        AST_NODE {_astNodeType = AST_Ignore, _astValue = Nothing, _astChildren = AST_VALUE [
                                            AST_NODE {_astNodeType = AST_Space, _astValue = Just " ", _astChildren = AST_VALUE []}
                                        ]},
                                        AST_NODE {_astNodeType = AST_FunctionParameterList, _astValue = Nothing, _astChildren = AST_VALUE []},
                                        AST_NODE {_astNodeType = AST_Ignore, _astValue = Nothing, _astChildren = AST_VALUE [
                                            AST_NODE {_astNodeType = AST_Space, _astValue = Just " ", _astChildren = AST_VALUE []}
                                        ]},
                                        AST_NODE {_astNodeType = AST_FunctionBody, _astValue = Nothing, _astChildren = AST_VALUE [
                                            AST_NODE {_astNodeType = AST_Number, _astValue = Nothing, _astChildren = AST_VALUE [
                                                AST_NODE {_astNodeType = AST_IntegerNumber, _astValue = Just "3", _astChildren = AST_VALUE []}
                                            ]}
                                        ]}
                                    ]},
                                    AST_NODE {_astNodeType = AST_Ignore, _astValue = Nothing, _astChildren = AST_VALUE [
                                        AST_NODE {_astNodeType = AST_Space, _astValue = Just " ", _astChildren = AST_VALUE []}
                                    ]}
                                ]
                                ,"xxx"
                            )
                    actual `shouldBe` expected

                it "{f (a b +) 3} xxx" $ do
                    let actual = functionDef "{f (a b +) 3} xxx"
                        expected = (
                                AST_VALUE [
                                    AST_NODE {_astNodeType = AST_Function, _astValue = Nothing, _astChildren = AST_VALUE [
                                        AST_NODE {_astNodeType = AST_Symbol, _astValue = Just "f", _astChildren = AST_VALUE []},
                                        AST_NODE {_astNodeType = AST_Ignore, _astValue = Nothing, _astChildren = AST_VALUE [
                                            AST_NODE {_astNodeType = AST_Space, _astValue = Just " ", _astChildren = AST_VALUE []}
                                        ]},
                                        AST_NODE {_astNodeType = AST_FunctionParameterList, _astValue = Nothing, _astChildren = AST_VALUE []},
                                        AST_NODE {_astNodeType = AST_Ignore, _astValue = Nothing, _astChildren = AST_VALUE [
                                            AST_NODE {_astNodeType = AST_Space, _astValue = Just " ", _astChildren = AST_VALUE []}
                                        ]},
                                        AST_NODE {_astNodeType = AST_FunctionBody, _astValue = Nothing, _astChildren = AST_VALUE [
                                            AST_NODE {_astNodeType = AST_Number, _astValue = Nothing, _astChildren = AST_VALUE [
                                                AST_NODE {_astNodeType = AST_IntegerNumber, _astValue = Just "3", _astChildren = AST_VALUE []}
                                            ]}
                                        ]}
                                    ]},
                                    AST_NODE {_astNodeType = AST_Ignore, _astValue = Nothing, _astChildren = AST_VALUE [
                                        AST_NODE {_astNodeType = AST_Space, _astValue = Just " ", _astChildren = AST_VALUE []}
                                    ]}
                                ]
                                ,"xxx"
                            )
                    actual `shouldBe` expected
