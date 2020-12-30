{-# LANGUAGE OverloadedStrings #-}

module AST2Spec where

    import Test.Hspec
    import Text.RawString.QQ
    import qualified Data.ByteString.Lazy.Char8 as L
    import AST2

    spec :: Spec
    spec = do
        describe "AST2" $ do
            describe "_string" $ do
                it "\"test\"xxx" $ do
                    let actual = _string "\"test\"xxx"
                        expected = (
                            AST_VALUE [AST_NODE {_astNodeType = AST_String, _astValue = Just "test", _astChildren = AST_VALUE []}]
                            ,"xxx"
                            )
                    actual `shouldBe` expected

                it "\"attr=\\\"value\\\"\"xxx" $ do
                    let actual = _string "\"attr=\\\"value\\\"\"xxx"
                        expected = (
                            AST_VALUE [AST_NODE {_astNodeType = AST_String, _astValue = Just "attr=\\\"value\\\"", _astChildren = AST_VALUE []}]
                            ,"xxx"
                            )
                    actual `shouldBe` expected

                it "\"line1\nline2\"xxx" $ do
                    let actual = _string "\"line1\nline2\"xxx"
                        expected = (
                            AST_VALUE [AST_NODE {_astNodeType = AST_String, _astValue = Just "line1\nline2", _astChildren = AST_VALUE []}],
                            "xxx"
                            )
                    actual `shouldBe` expected

                it "\"\"xxx" $ do
                    let actual = _string "\"\"xxx"
                        expected = (
                            AST_VALUE [AST_NODE {_astNodeType = AST_String, _astValue = Just "", _astChildren = AST_VALUE []}]
                            ,"xxx"
                            )
                    actual `shouldBe` expected

                it "\"🧐\" xxx" $ do
                    let actual = _string "\"🧐\" xxx"
                        expected = (
                            AST_VALUE [AST_NODE {_astNodeType = AST_String, _astValue = Just "🧐", _astChildren = AST_VALUE []}]
                            ," xxx"
                            )
                    actual `shouldBe` expected

            describe "_number" $ do
                -- it "123 xxx" $ do
                --     let actual = _number "123 xxx"
                --         expected = (
                --             AST_VALUE [
                --                 AST_NODE {_astNodeType = AST_Number, _astValue = Just "123", _astChildren = AST_VALUE []}]
                --                 ," xxx"
                --             )
                --     actual `shouldBe` expected

                -- it "123.222 xxx" $ do
                --     let actual = _number "123.222 xxx"
                --         expected = (
                --             AST_VALUE [
                --                 AST_NODE {_astNodeType = AST_Number, _astValue = Just "123.222", _astChildren = AST_VALUE []}]
                --                 ," xxx"
                --             )
                --     actual `shouldBe` expected

                -- it "-123.222 xxx" $ do
                --     let actual = _number "-123.222 xxx"
                --         expected = (
                --             AST_VALUE [
                --                 AST_NODE {_astNodeType = AST_Number, _astValue = Just "-123.222", _astChildren = AST_VALUE []}]
                --                 ," xxx"
                --             )
                --     actual `shouldBe` expected

                -- it "1xxx" $ do
                --     let actual = _number "1xxx"
                --         expected = (
                --                 AST_VALUE [
                --                     AST_NODE {
                --                         _astNodeType = AST_Number,
                --                         _astValue = Just "1",
                --                         _astChildren = AST_VALUE []
                --                     }
                --                 ],
                --                 "xxx"
                --             )
                --     actual `shouldBe` expected

                -- it "1/2 xxx" $ do
                --     let actual = _number "1/2 xxx"
                --         expected = (
                --             AST_VALUE [AST_NODE {_astNodeType = AST_Number, _astValue = Nothing, _astChildren = AST_VALUE [AST_NODE {_astNodeType = AST_IntegerNumber, _astValue = Just "1", _astChildren = AST_VALUE []},AST_NODE {_astNodeType = AST_Divide, _astValue = Just "/", _astChildren = AST_VALUE []},AST_NODE {_astNodeType = AST_NaturalNumber, _astValue = Just "2", _astChildren = AST_VALUE []}]}]
                --             ," xxx"
                --             )
                --     actual `shouldBe` expected

                it "-2/3 xxx" $ do
                    let actual = _number "-2/3 xxx"
                        expected = (
                            AST_VALUE [
                                AST_NODE {_astNodeType = AST_Number, _astValue = Nothing, _astChildren = AST_VALUE [
                                    AST_NODE {_astNodeType = AST_RationalNumber, _astValue = Nothing, _astChildren = AST_VALUE [
                                        AST_NODE {_astNodeType = AST_IntegerNumber, _astValue = Just "-2", _astChildren = AST_VALUE []},
                                        AST_NODE {_astNodeType = AST_Divide, _astValue = Just "/", _astChildren = AST_VALUE []},
                                        AST_NODE {_astNodeType = AST_NaturalNumber, _astValue = Just "3", _astChildren = AST_VALUE []}
                                    ]}]}
                            ]
                            ," xxx"
                            )
                    actual `shouldBe` expected

            describe "_complexNumber" $ do
                it "1+2i xxx" $ do
                    let actual = _complexNumber "1+2i xxx"
                        expected = (
                                AST_VALUE [
                                    AST_NODE {_astNodeType = AST_ComplexNumber, _astValue = Nothing, _astChildren = AST_VALUE [
                                        AST_NODE {_astNodeType = AST_IntegerNumber, _astValue = Just "1", _astChildren = AST_VALUE []}
                                        ,AST_NODE {_astNodeType = AST_Plus, _astValue = Just "+", _astChildren = AST_VALUE []},
                                        AST_NODE {_astNodeType = AST_IntegerNumber, _astValue = Just "2", _astChildren = AST_VALUE []},
                                        AST_NODE {_astNodeType = AST_ImaginaryUnit, _astValue = Just "i", _astChildren = AST_VALUE []}]}
                                    ]
                                , " xxx"
                            )
                    actual `shouldBe` expected

            --     it "1.5+2.5i xxx" $ do
            --         let actual = _complexNumber "1.5+2.5i xxx"
            --             expected = (
            --                     AST_VALUE [],
            --                     " xxx"
            --                 )
            --         actual `shouldBe` expected

            describe "_primitive" $ do
                it "13 xxx; 1/3 xxx; 1.3 xxx; 1+3i xxx" $ do
                    let actual = fmap (snd . _primitive) [
                                      "\"text\" xxx"
                                    , "13 xxx"
                                    , "1/3 xxx"
                                    , "1.3 xxx"
                                    , "1+3i xxx"
                                    ]
                        expected = take (length actual) (repeat " xxx")
                    actual `shouldBe` expected
