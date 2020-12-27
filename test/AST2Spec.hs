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
