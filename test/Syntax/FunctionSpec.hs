{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Syntax.FunctionSpec where

    import Test.Hspec
    import Text.RawString.QQ
    import qualified Data.ByteString.Lazy.Char8 as L
    import Tokenizer
    import Syntax

    spec :: Spec
    spec = do
        describe "Syntax for functions" $ do
            it "(a, b)" $ do
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

            it "{fn () 3}" $ do
                let actual = isFunction $ generateTokens "{fn () 3}"
                    expected =  ([
                                    AST_NODE {_astNodeType = AstFunction, _astTokens = [], _astChildren = [
                                        AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "fn", _TIndex = 1}], _astChildren = []},
                                        AST_NODE {_astNodeType = AstParameterList, _astTokens = [], _astChildren = []},
                                        AST_NODE {_astNodeType = AstFunctionBody, _astTokens = [], _astChildren = [
                                            AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_Number, _TValue = "3", _TIndex = 6}], _astChildren = []}
                                        ]}
                                    ]}
                                ],
                                [])

                actual `shouldBe` expected

            it "{fn () a}" $ do
                let actual = isFunction $ generateTokens "{fn () a}"
                    expected = ([
                                    AST_NODE {_astNodeType = AstFunction, _astTokens = [], _astChildren = [
                                        AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "fn", _TIndex = 1}], _astChildren = []},
                                        AST_NODE {_astNodeType = AstParameterList, _astTokens = [], _astChildren = []},
                                        AST_NODE {_astNodeType = AstFunctionBody, _astTokens = [], _astChildren = [
                                            AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "a", _TIndex = 6}], _astChildren = []}
                                        ]}
                                    ]}
                                ],
                                [])
                actual `shouldBe` expected

            it "{fn (a) (+ a 1)}" $ do
                let actual = isFunction $ generateTokens "{fn (a) (+ a 1)}"
                    expected = ([
                                AST_NODE {_astNodeType = AstFunction, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "fn", _TIndex = 1}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstParameterList, _astTokens = [], _astChildren = [
                                        AST_NODE {_astNodeType = AstParameter, _astTokens = [Token {_TType = T_Symbol, _TValue = "a", _TIndex = 4}], _astChildren = []}]},
                                    AST_NODE {_astNodeType = AstFunctionBody, _astTokens = [], _astChildren = [
                                        AST_NODE {_astNodeType = AstFunctionCall, _astTokens = [], _astChildren = [
                                            AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "+", _TIndex = 8}], _astChildren = []},
                                            AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "a", _TIndex = 10}], _astChildren = []},
                                            AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_Number, _TValue = "1", _TIndex = 12}], _astChildren = []}
                                        ]}
                                    ]}
                                ]}
                            ],[])
                actual `shouldBe` expected

            it "{(a) (a)}" $ do
                let actual = isLambda $ generateTokens "{(a) (a)}"
                    expected = ([
                                AST_NODE {_astNodeType = AstLambda, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstParameterList, _astTokens = [], _astChildren = [
                                        AST_NODE {_astNodeType = AstParameter, _astTokens = [Token {_TType = T_Symbol, _TValue = "a", _TIndex = 2}], _astChildren = []}]},
                                    AST_NODE {_astNodeType = AstFunctionBody, _astTokens = [], _astChildren = [
                                        AST_NODE {_astNodeType = AstFunctionCall, _astTokens = [], _astChildren = [
                                            AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "a", _TIndex = 6}], _astChildren = []}
                                        ]}
                                    ]}
                                ]}
                            ],[])
                actual `shouldBe` expected
