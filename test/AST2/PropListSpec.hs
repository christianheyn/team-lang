{-# LANGUAGE OverloadedStrings #-}

module AST2.PropListSpec where

    import Test.Hspec
    import Text.RawString.QQ
    import qualified Data.ByteString.Lazy.Char8 as L
    import AST2
    import Data.Maybe

    spec :: Spec
    spec = do
        describe "___propList" $ do
            it "[key-1: 5, key2: 'a'] xxx" $ do
                let actual = ___propList "[key-1: 5, key2: 'a'] xxx"
                    expected = (
                            AST_VALUE [
                                AST_NODE {_astNodeType = AST_PropList, _astValue = Nothing, _astChildren = AST_VALUE [
                                    AST_NODE {_astNodeType = AST_PropListKeyValue, _astValue = Nothing, _astChildren = AST_VALUE [
                                        AST_NODE {_astNodeType = AST_PropSymbol, _astValue = Just "key-1:", _astChildren = AST_VALUE []},
                                        AST_NODE {_astNodeType = AST_Number, _astValue = Nothing, _astChildren = AST_VALUE [
                                            AST_NODE {_astNodeType = AST_IntegerNumber, _astValue = Just "5", _astChildren = AST_VALUE []}
                                        ]}
                                    ]},
                                    AST_NODE {_astNodeType = AST_PropListKeyValue, _astValue = Nothing, _astChildren = AST_VALUE [
                                        AST_NODE {_astNodeType = AST_PropSymbol, _astValue = Just "key2:", _astChildren = AST_VALUE []},
                                        AST_NODE {_astNodeType = AST_Char, _astValue = Just "a", _astChildren = AST_VALUE []}
                                    ]}
                                ]}
                            ]
                            ,"xxx"
                        )
                actual `shouldBe` expected

            it "[key-1: [ key2: 'a' ], ] xxx" $ do
                let actual = ___propList "[key-1: [ key2: 'a' ], ] xxx"
                    expected = (
                            AST_VALUE [
                                AST_NODE {_astNodeType = AST_PropList, _astValue = Nothing, _astChildren = AST_VALUE [
                                    AST_NODE {_astNodeType = AST_PropListKeyValue, _astValue = Nothing, _astChildren = AST_VALUE [
                                        AST_NODE {_astNodeType = AST_PropSymbol, _astValue = Just "key-1:", _astChildren = AST_VALUE []},
                                        AST_NODE {_astNodeType = AST_PropList, _astValue = Nothing, _astChildren = AST_VALUE [
                                            AST_NODE {_astNodeType = AST_PropListKeyValue, _astValue = Nothing, _astChildren = AST_VALUE [
                                                AST_NODE {_astNodeType = AST_PropSymbol, _astValue = Just "key2:", _astChildren = AST_VALUE []},
                                                AST_NODE {_astNodeType = AST_Char, _astValue = Just "a", _astChildren = AST_VALUE []}
                                            ]}
                                        ]}
                                    ]}
                                ]}
                            ]
                            ,"xxx"
                        )
                actual `shouldBe` expected
