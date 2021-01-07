{-# LANGUAGE OverloadedStrings #-}

module AST2.TemplateTypeSpec where

    import Test.Hspec
    import Text.RawString.QQ
    import qualified Data.ByteString.Lazy.Char8 as L
    import AST2



    spec :: Spec
    spec = do
        describe "___templateType" $ do
            it "< T > xxx" $ do
                let actual = ___templateType "< T > xxx"
                    expected = (
                            AST_VALUE [
                                AST_NODE {_astNodeType = AST_TemplateType, _astValue = Nothing, _astChildren = AST_VALUE [
                                    AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "T", _astChildren = AST_VALUE []}
                                ]}
                            ]
                            ,"xxx"
                        )
                actual `shouldBe` expected

            it "<T>XXX" $ do
                let actual = ___templateType "<T>XXX"
                    expected = (
                            AST_VALUE [
                                AST_NODE {_astNodeType = AST_TemplateType, _astValue = Nothing, _astChildren = AST_VALUE [
                                    AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "T", _astChildren = AST_VALUE []}
                                ]}
                            ]
                            ,"XXX"
                        )
                actual `shouldBe` expected

            it "@ T xxx" $ do
                let actual = ___restType "@ T xxx"
                    expected = (
                            AST_VALUE [
                                AST_NODE {_astNodeType = AST_RestType, _astValue = Nothing, _astChildren = AST_VALUE [
                                    AST_NODE {_astNodeType = AST_TypeSymbol, _astValue = Just "T", _astChildren = AST_VALUE []}
                                ]}
                            ]
                            ,"xxx"
                        )
                actual `shouldBe` expected
