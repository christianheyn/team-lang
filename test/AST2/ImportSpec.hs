{-# LANGUAGE OverloadedStrings #-}

module AST2.ImportSpec where

    import Test.Hspec
    import Text.RawString.QQ
    import qualified Data.ByteString.Lazy.Char8 as L
    import AST2
    import Data.Maybe

    spec :: Spec
    spec = do
        describe "_import" $ do
            it "import (a b, c) from \"path/to/file.team\" xxx" $ do
                let actual = _import "import (a b, c) from \"path/to/file.team\" xxx"
                    expected = (
                            AST_VALUE [
                                AST_NODE {_astNodeType = AST_ImportFrom, _astValue = Nothing, _astChildren = AST_VALUE [
                                    AST_NODE {_astNodeType = AST_ImportList, _astValue = Nothing, _astChildren = AST_VALUE [
                                        AST_NODE {_astNodeType = AST_Symbol, _astValue = Just "a", _astChildren = AST_VALUE []},
                                        AST_NODE {_astNodeType = AST_Symbol, _astValue = Just "b", _astChildren = AST_VALUE []},
                                        AST_NODE {_astNodeType = AST_Symbol, _astValue = Just "c", _astChildren = AST_VALUE []}
                                    ]},
                                    AST_NODE {_astNodeType = AST_String, _astValue = Just "path/to/file.team", _astChildren = AST_VALUE []}
                                ]}
                            ]
                            ,"xxx"
                        )

                actual `shouldBe` expected

            it "import (a-b-c,\n b\n) from \n \"path/to/file.team\"; xxx" $ do
                let actual = _import "import (a-b-c,\n b\n) from \n \"path/to/file.team\"; xxx"
                    expected = (
                            AST_VALUE [
                                AST_NODE {_astNodeType = AST_ImportFrom, _astValue = Nothing, _astChildren = AST_VALUE [
                                    AST_NODE {_astNodeType = AST_ImportList, _astValue = Nothing, _astChildren = AST_VALUE [
                                        AST_NODE {_astNodeType = AST_Symbol, _astValue = Just "a-b-c", _astChildren = AST_VALUE []},
                                        AST_NODE {_astNodeType = AST_Symbol, _astValue = Just "b", _astChildren = AST_VALUE []}
                                    ]},
                                    AST_NODE {_astNodeType = AST_String, _astValue = Just "path/to/file.team", _astChildren = AST_VALUE []}
                                ]}
                            ]
                            ,"xxx"
                        )
                actual `shouldBe` expected

            it "import \"path/to/file.team\" as f; xxx" $ do
                let actual = _import "import \"path/to/file.team\" as f; xxx"
                    expected = (
                            AST_VALUE [
                                AST_NODE {_astNodeType = AST_ImportAs, _astValue = Nothing, _astChildren = AST_VALUE [
                                    AST_NODE {_astNodeType = AST_String, _astValue = Just "path/to/file.team", _astChildren = AST_VALUE []},
                                    AST_NODE {_astNodeType = AST_Symbol, _astValue = Just "f", _astChildren = AST_VALUE []}
                                ]}
                            ]
                            ,"xxx"
                        )
                actual `shouldBe` expected

            it "import \"path/to/file.team\" as xxx xxx" $ do
                let actual = _import "import \"path/to/file.team\" as xxx xxx"
                    expected = (
                            AST_VALUE [
                                AST_NODE {_astNodeType = AST_ImportAs, _astValue = Nothing, _astChildren = AST_VALUE [
                                    AST_NODE {_astNodeType = AST_String, _astValue = Just "path/to/file.team", _astChildren = AST_VALUE []},
                                    AST_NODE {_astNodeType = AST_Symbol, _astValue = Just "xxx", _astChildren = AST_VALUE []}
                                ]}
                            ]
                            ,"xxx"
                        )
                actual `shouldBe` expected

            it "import \"path/to/file.team\" as <//> xxx" $ do
                let actual = _import "import \"path/to/file.team\" as <//> xxx"
                    expected = (
                            AST_VALUE [
                                AST_NODE {_astNodeType = AST_ImportAs, _astValue = Nothing, _astChildren = AST_VALUE [
                                    AST_NODE {_astNodeType = AST_String, _astValue = Just "path/to/file.team", _astChildren = AST_VALUE []},
                                    AST_NODE {_astNodeType = AST_Symbol, _astValue = Just "<//>", _astChildren = AST_VALUE []}
                                ]}
                            ]
                            ,"xxx"
                        )
                actual `shouldBe` expected
