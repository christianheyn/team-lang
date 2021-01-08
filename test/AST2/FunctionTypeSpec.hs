{-# LANGUAGE OverloadedStrings #-}

module AST2.FunctionTypeSpec where

    import Test.Hspec
    import Text.RawString.QQ
    import qualified Data.ByteString.Lazy.Char8 as L
    import AST2
    import Data.Maybe

    spec :: Spec
    spec = do
        describe "___functionType" $ do
            it "{A -> B C -> maybe Number -> N} xxx" $ do
                let actual = ___functionType "{A -> B C -> maybe Number -> N} xxx"
                    expected = (
                            AST_VALUE [
                                AST_NODE {_astNodeType = AST_FunctionType, _astValue = Nothing, _astChildren = AST_VALUE [
                                    AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "A", _astChildren = AST_VALUE []},
                                    AST_NODE {_astNodeType = AST_WrapType, _astValue = Nothing, _astChildren = AST_VALUE [
                                        AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "B", _astChildren = AST_VALUE []},
                                        AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "C", _astChildren = AST_VALUE []}
                                    ]},
                                    AST_NODE {_astNodeType = AST_MaybeType, _astValue = Nothing, _astChildren = AST_VALUE [
                                        AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "Number", _astChildren = AST_VALUE []}
                                    ]},
                                    AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "N", _astChildren = AST_VALUE []}
                                ]}
                            ]
                            ," xxx"
                        )
                actual `shouldBe` expected

            it "{A (B C) -> @{N -> C} -> C} xxx" $ do
                let actual = ___functionType "{A (B C) -> @{N -> C} -> C} xxx"
                    expected = (
                            AST_VALUE [
                                AST_NODE {_astNodeType = AST_FunctionType, _astValue = Nothing, _astChildren = AST_VALUE [
                                    AST_NODE {_astNodeType = AST_WrapType, _astValue = Nothing, _astChildren = AST_VALUE [
                                        AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "A", _astChildren = AST_VALUE []},
                                        AST_NODE {_astNodeType = AST_WrapType, _astValue = Nothing, _astChildren = AST_VALUE [
                                            AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "B", _astChildren = AST_VALUE []},
                                            AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "C", _astChildren = AST_VALUE []}
                                        ]}
                                    ]},
                                    AST_NODE {_astNodeType = AST_RestType, _astValue = Nothing, _astChildren = AST_VALUE [
                                        AST_NODE {_astNodeType = AST_FunctionType, _astValue = Nothing, _astChildren = AST_VALUE [
                                            AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "N", _astChildren = AST_VALUE []},
                                            AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "C", _astChildren = AST_VALUE []}
                                        ]}
                                    ]},
                                    AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "C", _astChildren = AST_VALUE []}
                                ]}
                            ]
                            ," xxx"
                        )
                actual `shouldBe` expected

            it "{maybe N -> @ either A (B C) -> N} xxx" $ do
                let actual = ___functionType "{maybe N -> @ either A (B C) -> N} xxx"
                    expected = (
                            AST_VALUE [
                                AST_NODE {_astNodeType = AST_FunctionType, _astValue = Nothing, _astChildren = AST_VALUE [
                                    AST_NODE {_astNodeType = AST_MaybeType, _astValue = Nothing, _astChildren = AST_VALUE [
                                        AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "N", _astChildren = AST_VALUE []}
                                    ]},
                                    AST_NODE {_astNodeType = AST_RestType, _astValue = Nothing, _astChildren = AST_VALUE [
                                        AST_NODE {_astNodeType = AST_EitherType, _astValue = Nothing, _astChildren = AST_VALUE [
                                            AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "A", _astChildren = AST_VALUE []},
                                            AST_NODE {_astNodeType = AST_WrapType, _astValue = Nothing, _astChildren = AST_VALUE [
                                                AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "B", _astChildren = AST_VALUE []},
                                                AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "C", _astChildren = AST_VALUE []}
                                            ]}
                                        ]}
                                    ]},
                                    AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "N", _astChildren = AST_VALUE []}
                                ]}
                            ]
                            ," xxx"
                        )
                actual `shouldBe` expected

            it "<A><B> A -> B xxx" $ do
                let actual = typeDefinition "<A><B> A -> B xxx"
                    expected = (
                            AST_VALUE [
                                AST_NODE {_astNodeType = AST_TypeDefinition, _astValue = Nothing, _astChildren = AST_VALUE [
                                    AST_NODE {_astNodeType = AST_TemplateType, _astValue = Nothing, _astChildren = AST_VALUE [
                                        AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "A", _astChildren = AST_VALUE []}
                                    ]},
                                    AST_NODE {_astNodeType = AST_TemplateType, _astValue = Nothing, _astChildren = AST_VALUE [
                                        AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "B", _astChildren = AST_VALUE []}
                                    ]},
                                    AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "A", _astChildren = AST_VALUE []},
                                    AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "B", _astChildren = AST_VALUE []}
                                ]}
                            ]
                            ,"xxx"
                        )
                actual `shouldBe` expected

            it "<A><B> A -> B (xxx)" $ do
                let actual = typeDefinition "<A><B> A -> B (xxx)"
                    expected = (
                            AST_VALUE [
                                AST_NODE {_astNodeType = AST_TypeDefinition, _astValue = Nothing, _astChildren = AST_VALUE [
                                    AST_NODE {_astNodeType = AST_TemplateType, _astValue = Nothing, _astChildren = AST_VALUE [
                                        AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "A", _astChildren = AST_VALUE []}
                                    ]},
                                    AST_NODE {_astNodeType = AST_TemplateType, _astValue = Nothing, _astChildren = AST_VALUE [
                                        AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "B", _astChildren = AST_VALUE []}
                                    ]},
                                    AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "A", _astChildren = AST_VALUE []},
                                    AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "B", _astChildren = AST_VALUE []}
                                ]}
                            ]
                            ,"(xxx)"
                        )
                actual `shouldBe` expected
