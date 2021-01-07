{-# LANGUAGE OverloadedStrings #-}

module AST2.MaybeTypeSpec where

    import Test.Hspec
    import Text.RawString.QQ
    import qualified Data.ByteString.Lazy.Char8 as L
    import AST2
    import Data.Maybe

    spec :: Spec
    spec = do
        describe "___maybeType" $ do
            it "maybe Number xxx" $ do
                let actual = ___maybeType "maybe Number xxx"
                    expected = (
                            AST_VALUE [
                                AST_NODE {_astNodeType = AST_MaybeType, _astValue = Nothing, _astChildren = AST_VALUE [
                                    AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "Number", _astChildren = AST_VALUE []}
                                ]}
                            ]
                            ,"xxx"
                        )
                actual `shouldBe` expected

            it "maybe Number}xxx" $ do
                let actual = ___maybeType "maybe Number}xxx"
                    expected = (
                            AST_VALUE [
                                AST_NODE {_astNodeType = AST_MaybeType, _astValue = Nothing, _astChildren = AST_VALUE [
                                    AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "Number", _astChildren = AST_VALUE []}
                                ]}
                            ]
                            ,"}xxx"
                        )
                actual `shouldBe` expected

            it "maybe maybe Number xxx" $ do
                let actual = ___maybeType "maybe maybe Number xxx"
                    expected = (
                            AST_VALUE [
                                AST_NODE {_astNodeType = AST_MaybeType, _astValue = Nothing, _astChildren = AST_VALUE [
                                    AST_NODE {_astNodeType = AST_MaybeType, _astValue = Nothing, _astChildren = AST_VALUE [
                                        AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "Number", _astChildren = AST_VALUE []}
                                    ]}
                                ]}
                            ]
                            ,"xxx"
                        )
                actual `shouldBe` expected

            it "maybe lib.T xxx" $ do
                let actual = ___maybeType "maybe lib.T xxx"
                    expected = (
                            AST_VALUE [
                                AST_NODE {_astNodeType = AST_MaybeType, _astValue = Nothing, _astChildren = AST_VALUE [
                                    AST_NODE {_astNodeType = AST_ImportedTypeSymbol, _astValue = Nothing, _astChildren = AST_VALUE [
                                        AST_NODE {_astNodeType = AST_Symbol, _astValue = Just "lib", _astChildren = AST_VALUE []},
                                        AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "T", _astChildren = AST_VALUE []}
                                    ]}
                                ]}
                            ]
                            ,"xxx"
                        )

                actual `shouldBe` expected

            it "(maybe lib.T) xxx" $ do
                let actual = ___maybeType "(maybe lib.T) xxx"
                    expected = (
                            AST_VALUE [
                                AST_NODE {_astNodeType = AST_MaybeType, _astValue = Nothing, _astChildren = AST_VALUE [
                                    AST_NODE {_astNodeType = AST_ImportedTypeSymbol, _astValue = Nothing, _astChildren = AST_VALUE [
                                        AST_NODE {_astNodeType = AST_Symbol, _astValue = Just "lib", _astChildren = AST_VALUE []},
                                        AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "T", _astChildren = AST_VALUE []}
                                    ]}
                                ]}
                            ]
                            ,"xxx"
                        )

                actual `shouldBe` expected

            it "( maybe lib.T ) xxx" $ do
                let actual = ___maybeType "( maybe lib.T ) xxx"
                    expected = (
                            AST_VALUE [
                                AST_NODE {_astNodeType = AST_MaybeType, _astValue = Nothing, _astChildren = AST_VALUE [
                                    AST_NODE {_astNodeType = AST_ImportedTypeSymbol, _astValue = Nothing, _astChildren = AST_VALUE [
                                        AST_NODE {_astNodeType = AST_Symbol, _astValue = Just "lib", _astChildren = AST_VALUE []},
                                        AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "T", _astChildren = AST_VALUE []}
                                    ]}
                                ]}
                            ]
                            ,"xxx"
                        )

                actual `shouldBe` expected

            it "maybe A B C xxx" $ do
                let actual = ___maybeType "maybe A B C xxx"
                    expected = (
                            AST_VALUE [
                                AST_NODE {_astNodeType = AST_MaybeType, _astValue = Nothing, _astChildren = AST_VALUE [
                                    AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "A", _astChildren = AST_VALUE []}
                                ]}
                            ]
                            ,"B C xxx"
                        )

                actual `shouldBe` expected

            it "maybe (A B C) xxx" $ do
                let actual = ___maybeType "maybe (A B C) xxx"
                    expected = (
                            AST_VALUE [
                                AST_NODE {_astNodeType = AST_MaybeType, _astValue = Nothing, _astChildren = AST_VALUE [
                                    AST_NODE {_astNodeType = AST_WrapType, _astValue = Nothing, _astChildren = AST_VALUE [
                                        AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "A", _astChildren = AST_VALUE []},
                                        AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "B", _astChildren = AST_VALUE []},
                                        AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "C", _astChildren = AST_VALUE []}
                                    ]}
                                ]}
                            ]
                            ,"xxx"
                        )

                actual `shouldBe` expected
