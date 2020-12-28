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
                        expected = ("test", "xxx")
                    actual `shouldBe` expected

                it "\"attr=\\\"value\\\"\"xxx" $ do
                    let actual = __string "\"attr=\\\"value\\\"\"xxx"
                        expected = ("attr=\\\"value\\\"", "xxx")
                    actual `shouldBe` expected

                it "\"line1\nline2\"xxx" $ do
                    let actual = __string "\"line1\nline2\"xxx"
                        expected = ("line1\nline2", "xxx")
                    actual `shouldBe` expected

                it "\"\"xxx" $ do
                    let actual = __string "\"\"xxx"
                        expected = ("", "xxx")
                    actual `shouldBe` expected

                it "\"🧐\" xxx" $ do
                    let actual = __string "\"🧐\" xxx"
                        expected = ("🧐", " xxx")
                    actual `shouldBe` expected

            describe "__keyword" $ do
                it "import xxx" $ do
                    let actual = __keyword "import" "import xxx"
                        expected = ("import", " xxx")
                    actual `shouldBe` expected

                it "import(xxx" $ do
                    let actual = __keyword "import" "import(xxx"
                        expected = ("import", "(xxx")
                    actual `shouldBe` expected

                it "import(🧐" $ do
                    let actual = __keyword "import" "import(🧐"
                        expected = ("import", "(🧐")
                    actual `shouldBe` expected

            describe "__symbol" $ do
                it "xxx xxx" $ do
                    let actual = __symbol "xxx xxx"
                        expected = ("xxx", " xxx")
                    actual `shouldBe` expected

                it "~~µ??// xxx" $ do
                    let actual = __symbol "~~µ??// xxx"
                        expected = ("~~µ??//", " xxx")
                    actual `shouldBe` expected

                it "?🐵 xxx" $ do
                    let actual = __symbol "?🐵 xxx"
                        expected = ("?🐵", " xxx")
                    actual `shouldBe` expected

                it "aBC xxx" $ do
                    let actual = __symbol "aBC xxx"
                        expected = ("aBC", " xxx")
                    actual `shouldBe` expected

                it "1xx xxx" $ do
                    let actual = __symbol "1xx xxx"
                        expected = ("", "1xx xxx")
                    actual `shouldBe` expected

                it "-12 xxx" $ do
                    let actual = __symbol "-12 xxx"
                        expected = ("", "-12 xxx")
                    actual `shouldBe` expected

            describe "___naturalNumber" $ do
                it "123 xxx" $ do
                    let actual = ___naturalNumber "123 xxx"
                        expected = (
                                AST_VALUE [
                                    AST_NODE {
                                        _astNodeType = AST_NaturalNumber,
                                        _astValue = Just "123",
                                        _astChildren = AST_END
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
                                        _astNodeType = AST_NaturalNumber,
                                        _astValue = Just "1",
                                        _astChildren = AST_END
                                    }
                                ],
                                "xxx"
                            )
                    actual `shouldBe` expected