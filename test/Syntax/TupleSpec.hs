{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Syntax.TupleSpec where

    import Test.Hspec
    import Text.RawString.QQ
    import qualified Data.ByteString.Lazy.Char8 as L
    import Tokenizer
    import Syntax

    spec :: Spec
    spec = do
        describe "Syntax for tuples" $ do
            it "(pair 1 true)" $ do
                let actual = isTuple $ generateTokens "(pair 1 true)"
                    expected =  ([
                            AST_NODE {_astNodeType = AstPair, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_Number, _TValue = "1", _TIndex = 3}], _astChildren = []},
                                AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_BooleanTrue, _TValue = "true", _TIndex = 5}], _astChildren = []}
                            ]}
                        ],
                        [])
                actual `shouldBe` expected

            it "(pair 1+4i true)" $ do
                let actual = isTuple $ generateTokens "(pair 1+4i true)"
                    expected = ([
                            AST_NODE {_astNodeType = AstPair, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_ComplexNumber, _TValue = "1+4i", _TIndex = 3}], _astChildren = []},
                                AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_BooleanTrue, _TValue = "true", _TIndex = 5}], _astChildren = []}
                            ]}
                        ],
                        [])
                actual `shouldBe` expected

            it "pair 1+4i true" $ do
                let actual = isTuple $ generateTokens "pair 1+4i true"
                    expected = ([
                            AST_NODE {_astNodeType = AstPair, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_ComplexNumber, _TValue = "1+4i", _TIndex = 2}], _astChildren = []},
                                AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_BooleanTrue, _TValue = "true", _TIndex = 4}], _astChildren = []}
                            ]}
                        ],
                        [])
                actual `shouldBe` expected

            it "(triple 1 \"2\" true)" $ do
                let actual = isTuple $ generateTokens "(triple 1 \"2\" true)"
                    expected = ([
                            AST_NODE {_astNodeType = AstTriple, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_Number, _TValue = "1", _TIndex = 3}], _astChildren = []},
                                AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_String, _TValue = "\"2\"", _TIndex = 5}], _astChildren = []},
                                AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_BooleanTrue, _TValue = "true", _TIndex = 7}], _astChildren = []}
                            ]}
                        ],
                        [])
                actual `shouldBe` expected

            it "triple 1 \"2\" true" $ do
                let actual = isTuple $ generateTokens "triple 1 \"2\" true"
                    expected = ([
                            AST_NODE {_astNodeType = AstTriple, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_Number, _TValue = "1", _TIndex = 2}], _astChildren = []},
                                AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_String, _TValue = "\"2\"", _TIndex = 4}], _astChildren = []},
                                AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_BooleanTrue, _TValue = "true", _TIndex = 6}], _astChildren = []}
                            ]}
                        ],
                        [])
                actual `shouldBe` expected

            it "pair 1+4i triple false void 3+7i" $ do
                let actual = isTuple $ generateTokens "pair 1+4i triple false void 3+7i"
                    expected = ([
                            AST_NODE {_astNodeType = AstPair, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_ComplexNumber, _TValue = "1+4i", _TIndex = 2}], _astChildren = []},
                                AST_NODE {_astNodeType = AstTriple, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_BooleanFalse, _TValue = "false", _TIndex = 6}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_Void, _TValue = "void", _TIndex = 8}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_ComplexNumber, _TValue = "3+7i", _TIndex = 10}], _astChildren = []}
                                ]}
                            ]}
                        ],
                        [])
                actual `shouldBe` expected
