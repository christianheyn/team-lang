{-# LANGUAGE OverloadedStrings #-}

module AST2.EitherTypeSpec where

    import Test.Hspec
    import Text.RawString.QQ
    import qualified Data.ByteString.Lazy.Char8 as L
    import AST2
    import Data.Maybe

    spec :: Spec
    spec = do
        describe "___eitherType" $ do
            it "either Number String xxx" $ do
                let actual = ___eitherType "either Number String xxx"
                    expected = (
                            AST_VALUE [
                                AST_NODE {_astNodeType = AST_EitherType, _astValue = Nothing, _astChildren = AST_VALUE [
                                    AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "Number", _astChildren = AST_VALUE []},
                                    AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "String", _astChildren = AST_VALUE []}
                                ]}
                            ]
                            ,"xxx"
                        )
                actual `shouldBe` expected

            it "either (N)    T U  V W xxx" $ do
                let actual = ___eitherType "either (N)    T U  V W xxx"
                    expected = (
                            AST_VALUE [
                                AST_NODE {_astNodeType = AST_EitherType, _astValue = Nothing, _astChildren = AST_VALUE [
                                    AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "N", _astChildren = AST_VALUE []},
                                    AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "T", _astChildren = AST_VALUE []},
                                    AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "U", _astChildren = AST_VALUE []},
                                    AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "V", _astChildren = AST_VALUE []},
                                    AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "W", _astChildren = AST_VALUE []}
                                ]}
                            ]
                            ,"xxx"
                        )
                actual `shouldBe` expected

            it "either maybe T U (maybe V) xxx" $ do
                let actual = ___eitherType "either maybe T U (maybe V) xxx"
                    expected = (
                            AST_VALUE [
                                AST_NODE {_astNodeType = AST_EitherType, _astValue = Nothing, _astChildren = AST_VALUE [
                                    AST_NODE {_astNodeType = AST_MaybeType, _astValue = Nothing, _astChildren = AST_VALUE [
                                        AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "T", _astChildren = AST_VALUE []}
                                    ]},
                                    AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "U", _astChildren = AST_VALUE []},
                                    AST_NODE {_astNodeType = AST_MaybeType, _astValue = Nothing, _astChildren = AST_VALUE [
                                        AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "V", _astChildren = AST_VALUE []}
                                    ]}
                                ]}
                            ]
                            ,"xxx"
                        )
                actual `shouldBe` expected

            it "(either maybe T U (maybe V)) xxx" $ do
                let actual = ___eitherType "(either maybe T U (maybe V)) xxx"
                    expected = (
                            AST_VALUE [
                                AST_NODE {_astNodeType = AST_EitherType, _astValue = Nothing, _astChildren = AST_VALUE [
                                    AST_NODE {_astNodeType = AST_MaybeType, _astValue = Nothing, _astChildren = AST_VALUE [
                                        AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "T", _astChildren = AST_VALUE []}
                                    ]},
                                    AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "U", _astChildren = AST_VALUE []},
                                    AST_NODE {_astNodeType = AST_MaybeType, _astValue = Nothing, _astChildren = AST_VALUE [
                                        AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "V", _astChildren = AST_VALUE []}
                                    ]}
                                ]}
                            ]
                            ,"xxx"
                        )
                actual `shouldBe` expected

            it "either [a: N, b: B A N] [b: N] xxx" $ do
                let actual = ___eitherType "either [a: N, b: B A N] [b: N] xxx"
                    expected = (
                            AST_VALUE [
                                AST_NODE {_astNodeType = AST_EitherType, _astValue = Nothing, _astChildren = AST_VALUE [
                                    AST_NODE {_astNodeType = AST_PropListType, _astValue = Nothing, _astChildren = AST_VALUE [
                                        AST_NODE {_astNodeType = AST_PropListKeyValue, _astValue = Nothing, _astChildren = AST_VALUE [
                                            AST_NODE {_astNodeType = AST_PropSymbol, _astValue = Just "a:", _astChildren = AST_VALUE []},
                                            AST_NODE {_astNodeType = AST_TypeDefinition, _astValue = Nothing, _astChildren = AST_VALUE [
                                                AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "N", _astChildren = AST_VALUE []}
                                            ]}
                                        ]},
                                        AST_NODE {_astNodeType = AST_PropListKeyValue, _astValue = Nothing, _astChildren = AST_VALUE [
                                            AST_NODE {_astNodeType = AST_PropSymbol, _astValue = Just "b:", _astChildren = AST_VALUE []},
                                            AST_NODE {_astNodeType = AST_TypeDefinition, _astValue = Nothing, _astChildren = AST_VALUE [
                                                AST_NODE {_astNodeType = AST_WrapType, _astValue = Nothing, _astChildren = AST_VALUE [
                                                    AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "B", _astChildren = AST_VALUE []},
                                                    AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "A", _astChildren = AST_VALUE []},
                                                    AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "N", _astChildren = AST_VALUE []}
                                                ]}
                                            ]}
                                        ]}
                                    ]},
                                    AST_NODE {_astNodeType = AST_PropListType, _astValue = Nothing, _astChildren = AST_VALUE [
                                        AST_NODE {_astNodeType = AST_PropListKeyValue, _astValue = Nothing, _astChildren = AST_VALUE [
                                            AST_NODE {_astNodeType = AST_PropSymbol, _astValue = Just "b:", _astChildren = AST_VALUE []},
                                            AST_NODE {_astNodeType = AST_TypeDefinition, _astValue = Nothing, _astChildren = AST_VALUE [
                                                AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "N", _astChildren = AST_VALUE []}
                                            ]}
                                        ]}
                                    ]}
                                ]}
                            ]
                            ,"xxx"
                        )
                actual `shouldBe` expected
