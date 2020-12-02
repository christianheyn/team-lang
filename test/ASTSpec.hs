{-# LANGUAGE OverloadedStrings #-}

module ASTSpec where

    import Test.Hspec
    import Text.RawString.QQ
    import qualified Data.ByteString.Lazy.Char8 as L
    import Tokenizer
    import Syntax

    spec :: Spec
    spec = do
        describe "Syntax" $ do
            it "Parameter: \"(a, b)\"" $ do
                let actual = isParamterList $ generateTokens "()"
                    expected = (
                        [AST_NODE {
                              _astNodeType = AstParameterList
                            , _astTokens = []
                            , _astChildren = []
                            }
                        ],
                        []
                        )
                actual `shouldBe` expected

            it "List: \"[1 [2 [3 true]]]\"" $ do
                let actual = isList $ generateTokens "[1 [2 [3 true]]]"
                    expected = (
                        [
                            AST_NODE {_astNodeType = AstList, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_Number, _TValue = "1", _TIndex = 1}], _astChildren = []},
                                AST_NODE {_astNodeType = AstList, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_Number, _TValue = "2", _TIndex = 4}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstList, _astTokens = [], _astChildren = [
                                        AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_Number, _TValue = "3", _TIndex = 7}], _astChildren = []},
                                        AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_BooleanTrue, _TValue = "true", _TIndex = 9}], _astChildren = []}
                                    ]}
                                ]}
                            ]}
                        ],
                        []
                        )
                actual `shouldBe` expected
