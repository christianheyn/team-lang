{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Syntax.ClassInstanceSpec where

    import Test.Hspec
    import Text.RawString.QQ
    import qualified Data.ByteString.Lazy.Char8 as L
    import Tokenizer
    import Syntax

    spec :: Spec
    spec = do
        describe "Syntax for class instance" $ do
            it "instance Functor F { {fmap () (a)} }" $ do
                let actual = isClassInstance $ generateTokens "instance Functor F { {fmap () (a)} }"
                    expected = ([
                            AST_NODE {_astNodeType = AstClassInstance, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "Functor", _TIndex = 2}], _astChildren = []},
                                AST_NODE {_astNodeType = AstTypeDefinition, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "F", _TIndex = 4}], _astChildren = []}
                                ]},
                                AST_NODE {_astNodeType = AstOpen, _astTokens = [Token {_TType = T_OpenCurlyBracket, _TValue = "{", _TIndex = 6}], _astChildren = []},
                                AST_NODE {_astNodeType = AstFunction, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "fmap", _TIndex = 9}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstParameterList, _astTokens = [], _astChildren = []},
                                    AST_NODE {_astNodeType = AstFunctionBody, _astTokens = [], _astChildren = [
                                        AST_NODE {_astNodeType = AstFunctionCall, _astTokens = [], _astChildren = [
                                            AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "a", _TIndex = 15}], _astChildren = []}
                                        ]}
                                    ]}
                                ]},
                                AST_NODE {_astNodeType = AstClose, _astTokens = [Token {_TType = T_ClosingCurlyBracket, _TValue = "}", _TIndex = 19}], _astChildren = []}
                            ]}
                        ],
                        [])
                actual `shouldBe` expected

            it "instance Functor [a: Number] { {fmap (f x) (over a: f x)} }" $ do
                let actual = isClassInstance $ generateTokens "instance Functor [a: Number] { {fmap (f x) (over a: f x)} }"
                    expected = ([
                            AST_NODE {_astNodeType = AstClassInstance, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "Functor", _TIndex = 2}], _astChildren = []},
                                AST_NODE {_astNodeType = AstTypeDefinition, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstPropListType, _astTokens = [], _astChildren = [
                                        AST_NODE {_astNodeType = AstPropKeyValueType, _astTokens = [], _astChildren = [
                                            AST_NODE {_astNodeType = AstProp, _astTokens = [Token {_TType = T_Prop, _TValue = "a:", _TIndex = 5}], _astChildren = []},
                                            AST_NODE {_astNodeType = AstTypeDefinition, _astTokens = [], _astChildren = [
                                                AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "Number", _TIndex = 7}], _astChildren = []}
                                            ]}
                                        ]}
                                    ]}
                                ]},
                                AST_NODE {_astNodeType = AstOpen, _astTokens = [Token {_TType = T_OpenCurlyBracket, _TValue = "{", _TIndex = 10}], _astChildren = []},
                                AST_NODE {_astNodeType = AstFunction, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "fmap", _TIndex = 13}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstParameterList, _astTokens = [], _astChildren = [
                                        AST_NODE {_astNodeType = AstParameter, _astTokens = [Token {_TType = T_Symbol, _TValue = "f", _TIndex = 16}], _astChildren = []},
                                        AST_NODE {_astNodeType = AstParameter, _astTokens = [Token {_TType = T_Symbol, _TValue = "x", _TIndex = 18}], _astChildren = []}
                                    ]},
                                    AST_NODE {_astNodeType = AstFunctionBody, _astTokens = [], _astChildren = [
                                        AST_NODE {_astNodeType = AstFunctionCall, _astTokens = [], _astChildren = [
                                            AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "over", _TIndex = 22}], _astChildren = []},
                                            AST_NODE {_astNodeType = AstProp, _astTokens = [Token {_TType = T_Prop, _TValue = "a:", _TIndex = 24}], _astChildren = []},
                                            AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "f", _TIndex = 26}], _astChildren = []},
                                            AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "x", _TIndex = 28}], _astChildren = []}
                                        ]}
                                    ]}
                                ]},
                                AST_NODE {_astNodeType = AstClose, _astTokens = [Token {_TType = T_ClosingCurlyBracket, _TValue = "}", _TIndex = 32}], _astChildren = []}
                            ]}
                        ],
                        [])
                actual `shouldBe` expected
