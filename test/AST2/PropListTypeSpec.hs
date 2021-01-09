{-# LANGUAGE OverloadedStrings #-}

module AST2.PropListTypeSpec where

    import Test.Hspec
    import Text.RawString.QQ
    import qualified Data.ByteString.Lazy.Char8 as L
    import AST2
    import Data.Maybe

    spec :: Spec
    spec = do
        describe "___propListType" $ do
            it "[key-1: Number] xxx" $ do
                let actual = ___propListType "[key-1: Number] xxx"
                    expected = (
                            AST_VALUE [
                                AST_NODE {_astNodeType = AST_PropListType, _astValue = Nothing, _astChildren = AST_VALUE [
                                    AST_NODE {_astNodeType = AST_PropListKeyValue, _astValue = Nothing, _astChildren = AST_VALUE [
                                        AST_NODE {_astNodeType = AST_PropSymbol, _astValue = Just "key-1:", _astChildren = AST_VALUE []},
                                        AST_NODE {_astNodeType = AST_TypeDefinition, _astValue = Nothing, _astChildren = AST_VALUE [
                                            AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "Number", _astChildren = AST_VALUE []}
                                        ]}
                                    ]}
                                ]}
                            ]
                            ,"xxx"
                        )
                actual `shouldBe` expected
