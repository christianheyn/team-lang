{-# LANGUAGE OverloadedStrings #-}

module AST2.FunctionSpec where

    import Test.Hspec
    import Text.RawString.QQ
    import qualified Data.ByteString.Lazy.Char8 as L
    import AST2

    spec :: Spec
    spec = do
        describe "AST2 functions" $ do
            describe "function" $ do
                it "fun f () 3 xxx" $ do
                    let actual = function "fun f () 3 xxx"
                        expected = (
                                AST_VALUE [
                                    AST_NODE {_astNodeType = AST_Function, _astValue = Nothing, _astChildren = AST_VALUE [
                                        AST_NODE {_astNodeType = AST_Symbol, _astValue = Just "f", _astChildren = AST_VALUE []},
                                        AST_NODE {_astNodeType = AST_FunctionParameterList, _astValue = Nothing, _astChildren = AST_VALUE []},
                                        AST_NODE {_astNodeType = AST_FunctionBody, _astValue = Nothing, _astChildren = AST_VALUE [
                                            AST_NODE {_astNodeType = AST_Number, _astValue = Nothing, _astChildren = AST_VALUE [
                                                AST_NODE {_astNodeType = AST_IntegerNumber, _astValue = Just "3", _astChildren = AST_VALUE []}
                                            ]}
                                        ]}
                                    ]}
                                ]
                                ,"xxx"
                            )
                    actual `shouldBe` expected

                it "{f () 3} xxx" $ do
                    let actual = function "{f () 3} xxx"
                        expected = (
                                AST_VALUE [
                                    AST_NODE {_astNodeType = AST_Function, _astValue = Nothing, _astChildren = AST_VALUE [
                                        AST_NODE {_astNodeType = AST_Symbol, _astValue = Just "f", _astChildren = AST_VALUE []},
                                        AST_NODE {_astNodeType = AST_FunctionParameterList, _astValue = Nothing, _astChildren = AST_VALUE []},
                                        AST_NODE {_astNodeType = AST_FunctionBody, _astValue = Nothing, _astChildren = AST_VALUE [
                                            AST_NODE {_astNodeType = AST_Number, _astValue = Nothing, _astChildren = AST_VALUE [
                                                AST_NODE {_astNodeType = AST_IntegerNumber, _astValue = Just "3", _astChildren = AST_VALUE []}
                                            ]}
                                        ]}
                                    ]}
                                ]
                                ,"xxx"
                            )
                    actual `shouldBe` expected

                it "{f (a b +) 3} xxx" $ do
                    let actual = function "{f (a b +) 3} xxx"
                        expected = (
                                AST_VALUE [
                                    AST_NODE {_astNodeType = AST_Function, _astValue = Nothing, _astChildren = AST_VALUE [
                                        AST_NODE {_astNodeType = AST_Symbol, _astValue = Just "f", _astChildren = AST_VALUE []},
                                        AST_NODE {_astNodeType = AST_FunctionParameterList, _astValue = Nothing, _astChildren = AST_VALUE [
                                            AST_NODE {_astNodeType = AST_Symbol, _astValue = Just "a", _astChildren = AST_VALUE []},
                                            AST_NODE {_astNodeType = AST_Symbol, _astValue = Just "b", _astChildren = AST_VALUE []},
                                            AST_NODE {_astNodeType = AST_Symbol, _astValue = Just "+", _astChildren = AST_VALUE []}
                                        ]},
                                        AST_NODE {_astNodeType = AST_FunctionBody, _astValue = Nothing, _astChildren = AST_VALUE [
                                            AST_NODE {_astNodeType = AST_Number, _astValue = Nothing, _astChildren = AST_VALUE [
                                                AST_NODE {_astNodeType = AST_IntegerNumber, _astValue = Just "3", _astChildren = AST_VALUE []}
                                            ]}
                                        ]}
                                    ]}
                                ]
                                ,"xxx"
                            )
                    actual `shouldBe` expected

                it "{f (a, b) 3} xxx" $ do
                    let actual = function "{f (a, b) 3} xxx"
                        expected = (
                                AST_VALUE [
                                    AST_NODE {_astNodeType = AST_Function, _astValue = Nothing, _astChildren = AST_VALUE [
                                        AST_NODE {_astNodeType = AST_Symbol, _astValue = Just "f", _astChildren = AST_VALUE []},
                                        AST_NODE {_astNodeType = AST_FunctionParameterList, _astValue = Nothing, _astChildren = AST_VALUE [
                                            AST_NODE {_astNodeType = AST_Symbol, _astValue = Just "a", _astChildren = AST_VALUE []},
                                            AST_NODE {_astNodeType = AST_Symbol, _astValue = Just "b", _astChildren = AST_VALUE []}
                                        ]},
                                        AST_NODE {_astNodeType = AST_FunctionBody, _astValue = Nothing, _astChildren = AST_VALUE [
                                            AST_NODE {_astNodeType = AST_Number, _astValue = Nothing, _astChildren = AST_VALUE [
                                                AST_NODE {_astNodeType = AST_IntegerNumber, _astValue = Just "3", _astChildren = AST_VALUE []}
                                            ]}
                                        ]}
                                    ]}
                                ]
                                ,"xxx"
                            )

                    actual `shouldBe` expected


            describe "lambda" $ do
                it "{(a) (+ 1 4)} xxx" $ do
                    let actual = lambda "{(a) (+ 1 4)} xxx"
                        expected = (
                                AST_VALUE [
                                    AST_NODE {_astNodeType = AST_Lambda, _astValue = Nothing, _astChildren = AST_VALUE [
                                        AST_NODE {_astNodeType = AST_FunctionParameterList, _astValue = Nothing, _astChildren = AST_VALUE [
                                            AST_NODE {_astNodeType = AST_Symbol, _astValue = Just "a", _astChildren = AST_VALUE []}
                                        ]},
                                        AST_NODE {_astNodeType = AST_FunctionBody, _astValue = Nothing, _astChildren = AST_VALUE [
                                            AST_NODE {_astNodeType = AST_FunctionCall, _astValue = Nothing, _astChildren = AST_VALUE [
                                                AST_NODE {_astNodeType = AST_Symbol, _astValue = Just "+", _astChildren = AST_VALUE []},
                                                AST_NODE {_astNodeType = AST_Number, _astValue = Nothing, _astChildren = AST_VALUE [
                                                    AST_NODE {_astNodeType = AST_IntegerNumber, _astValue = Just "1", _astChildren = AST_VALUE []}
                                                ]},
                                                AST_NODE {_astNodeType = AST_Number, _astValue = Nothing, _astChildren = AST_VALUE [
                                                    AST_NODE {_astNodeType = AST_IntegerNumber, _astValue = Just "4", _astChildren = AST_VALUE []}
                                                ]}
                                            ]}
                                        ]}
                                    ]}
                                ]
                                ,"xxx"
                            )

                    actual `shouldBe` expected

            describe "functionCall" $ do
                it "(f 3 \"text\" 2.3+4i) xxx" $ do
                    let actual = functionCall "(f 3 \"text\" 2.3+4i) xxx"
                        expected = (
                                AST_VALUE [
                                    AST_NODE {_astNodeType = AST_FunctionCall, _astValue = Nothing, _astChildren = AST_VALUE [
                                        AST_NODE {_astNodeType = AST_Symbol, _astValue = Just "f", _astChildren = AST_VALUE []},
                                        AST_NODE {_astNodeType = AST_Number, _astValue = Nothing, _astChildren = AST_VALUE [
                                            AST_NODE {_astNodeType = AST_IntegerNumber, _astValue = Just "3", _astChildren = AST_VALUE []}
                                        ]},
                                        AST_NODE {_astNodeType = AST_String, _astValue = Just "text", _astChildren = AST_VALUE []},
                                        AST_NODE {_astNodeType = AST_ComplexNumber, _astValue = Nothing, _astChildren = AST_VALUE [
                                            AST_NODE {_astNodeType = AST_RealNumber, _astValue = Nothing, _astChildren = AST_VALUE [
                                                AST_NODE {_astNodeType = AST_IntegerNumber, _astValue = Just "2", _astChildren = AST_VALUE []},
                                                AST_NODE {_astNodeType = AST_NaturalNumber, _astValue = Just "3", _astChildren = AST_VALUE []}
                                            ]},
                                            AST_NODE {_astNodeType = AST_IntegerNumber, _astValue = Just "4", _astChildren = AST_VALUE []}
                                        ]}
                                    ]}
                                ]
                                ,"xxx"
                            )
                    actual `shouldBe` expected

                it "(f 3, \"text\", 2.3+4i,) xxx" $ do
                    let actual = functionCall "(f 3, \"text\", 2.3+4i,) xxx"
                        expected = (
                                AST_VALUE [
                                    AST_NODE {_astNodeType = AST_FunctionCall, _astValue = Nothing, _astChildren = AST_VALUE [
                                        AST_NODE {_astNodeType = AST_Symbol, _astValue = Just "f", _astChildren = AST_VALUE []},
                                        AST_NODE {_astNodeType = AST_Number, _astValue = Nothing, _astChildren = AST_VALUE [
                                            AST_NODE {_astNodeType = AST_IntegerNumber, _astValue = Just "3", _astChildren = AST_VALUE []}
                                        ]},
                                        AST_NODE {_astNodeType = AST_String, _astValue = Just "text", _astChildren = AST_VALUE []},
                                        AST_NODE {_astNodeType = AST_ComplexNumber, _astValue = Nothing, _astChildren = AST_VALUE [
                                            AST_NODE {_astNodeType = AST_RealNumber, _astValue = Nothing, _astChildren = AST_VALUE [
                                                AST_NODE {_astNodeType = AST_IntegerNumber, _astValue = Just "2", _astChildren = AST_VALUE []},
                                                AST_NODE {_astNodeType = AST_NaturalNumber, _astValue = Just "3", _astChildren = AST_VALUE []}
                                            ]},
                                            AST_NODE {_astNodeType = AST_IntegerNumber, _astValue = Just "4", _astChildren = AST_VALUE []}
                                        ]}
                                    ]}
                                ]
                                ,"xxx"
                            )
                    actual `shouldBe` expected
