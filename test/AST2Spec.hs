{-# LANGUAGE OverloadedStrings #-}

module AST2Spec where

    import Test.Hspec
    import Text.RawString.QQ
    import qualified Data.ByteString.Lazy.Char8 as L
    import AST2

    spec :: Spec
    spec = do
        describe "AST2" $ do
            describe "__string" $ do
                it "\"test\"xxx" $ do
                    let actual = __string "\"test\"xxx"
                        expected = (
                            AST_VALUE [AST_NODE {_astNodeType = AST_String, _astValue = Just "test", _astChildren = AST_VALUE []}]
                            ,"xxx"
                            )
                    actual `shouldBe` expected

                it "\"attr=\\\"value\\\"\"xxx" $ do
                    let actual = __string "\"attr=\\\"value\\\"\"xxx"
                        expected = (
                            AST_VALUE [AST_NODE {_astNodeType = AST_String, _astValue = Just "attr=\\\"value\\\"", _astChildren = AST_VALUE []}]
                            ,"xxx"
                            )
                    actual `shouldBe` expected

                it "\"line1\nline2\"xxx" $ do
                    let actual = __string "\"line1\nline2\"xxx"
                        expected = (
                            AST_VALUE [AST_NODE {_astNodeType = AST_String, _astValue = Just "line1\nline2", _astChildren = AST_VALUE []}],
                            "xxx"
                            )
                    actual `shouldBe` expected

                it "\"\"xxx" $ do
                    let actual = __string "\"\"xxx"
                        expected = (
                            AST_VALUE [AST_NODE {_astNodeType = AST_String, _astValue = Just "", _astChildren = AST_VALUE []}]
                            ,"xxx"
                            )
                    actual `shouldBe` expected

                it "\"🧐\" xxx" $ do
                    let actual = __string "\"🧐\" xxx"
                        expected = (
                            AST_VALUE [AST_NODE {_astNodeType = AST_String, _astValue = Just "🧐", _astChildren = AST_VALUE []}]
                            ," xxx"
                            )
                    actual `shouldBe` expected

            describe "___decimalNumber" $ do
                it "123.222 xxx" $ do
                    let actual = ___decimalNumber "123.222 xxx"
                        expected = (
                                AST_VALUE [
                                    AST_NODE {_astNodeType = AST_Number, _astValue = Just "123", _astChildren = AST_VALUE []},
                                    AST_NODE {_astNodeType = AST_Number, _astValue = Just "222", _astChildren = AST_VALUE []}
                                ],
                                " xxx"
                            )
                    actual `shouldBe` expected

                it "-123.222 xxx" $ do
                    let actual = ___decimalNumber "-123.222 xxx"
                        expected = (
                                AST_VALUE [],
                                " xxx"
                            )
                    actual `shouldBe` expected

            describe "___naturalNumber" $ do
                it "123 xxx" $ do
                    let actual = ___naturalNumber "123 xxx"
                        expected = (
                                AST_VALUE [
                                    AST_NODE {
                                        _astNodeType = AST_Number,
                                        _astValue = Just "123",
                                        _astChildren = AST_VALUE []
                                    }
                                ],
                                " xxx"
                            )
                    actual `shouldBe` expected

                it "1xxx" $ do
                    let actual = ___naturalNumber "1xxx"
                        expected = (
                                AST_VALUE [
                                    AST_NODE {
                                        _astNodeType = AST_Number,
                                        _astValue = Just "1",
                                        _astChildren = AST_VALUE []
                                    }
                                ],
                                "xxx"
                            )
                    actual `shouldBe` expected
