{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Syntax.ListSpec where

    import Test.Hspec
    import Text.RawString.QQ
    import qualified Data.ByteString.Lazy.Char8 as L
    import Tokenizer
    import Syntax

    spec :: Spec
    spec = do
        describe "Syntax for Lists" $ do
            it "[1 [2 [3 true]]]" $ do
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

            it "[1+3i 1+2i]" $ do
                let actual = isList $ generateTokens "[1+3i 1+2i]"
                    expected = ([
                            AST_NODE {_astNodeType = AstList, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_ComplexNumber, _TValue = "1+3i", _TIndex = 1}], _astChildren = []},
                                AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_ComplexNumber, _TValue = "1+2i", _TIndex = 3}], _astChildren = []}
                            ]}
                        ],
                        [])
                actual `shouldBe` expected
