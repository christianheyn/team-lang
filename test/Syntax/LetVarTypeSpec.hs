{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Syntax.LetVarTypeSpec where

    import Test.Hspec
    import Text.RawString.QQ
    import qualified Data.ByteString.Lazy.Char8 as L
    import Tokenizer
    import Syntax

    spec :: Spec
    spec = do
        describe "Syntax for let, var, type" $ do
            it "(var Number a 3)" $ do
                let actual = isVar $ generateTokens "(var Number a 3)"
                    expected = ([
                            AST_NODE {_astNodeType = AstVar, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstTypeDefinition, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "Number", _TIndex = 3}], _astChildren = []}
                                ]},
                                AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "a", _TIndex = 5}], _astChildren = []},
                                AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_Number, _TValue = "3", _TIndex = 7}], _astChildren = []}
                            ]}
                        ],
                        [])
                actual `shouldBe` expected

            it "(var maybe Number a void)" $ do
                let actual = isVar $ generateTokens "(var maybe Number a void)"
                    expected = ([
                            AST_NODE {_astNodeType = AstVar, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstTypeDefinition, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstMaybeType, _astTokens = [], _astChildren = [
                                        AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "Number", _TIndex = 5}], _astChildren = []}
                                    ]}
                                ]},
                                AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "a", _TIndex = 7}], _astChildren = []},
                                AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_Void, _TValue = "void", _TIndex = 9}], _astChildren = []}
                            ]}
                        ],
                        [])
                actual `shouldBe` expected

            it "(var Number a (if (< b 3) then 4 else 2))" $ do
                let actual = isVar $ generateTokens "(var Number a (if (< b 3) then 4 else 2))"
                    expected = ([
                            AST_NODE {_astNodeType = AstVar, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstTypeDefinition, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "Number", _TIndex = 3}], _astChildren = []}
                                ]},
                                AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "a", _TIndex = 5}], _astChildren = []},
                                AST_NODE {_astNodeType = AstIfCondition, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstFunctionCall, _astTokens = [], _astChildren = [
                                        AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "<", _TIndex = 11}], _astChildren = []},
                                        AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "b", _TIndex = 13}], _astChildren = []},
                                        AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_Number, _TValue = "3", _TIndex = 15}], _astChildren = []}
                                    ]},
                                    AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_Number, _TValue = "4", _TIndex = 20}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_Number, _TValue = "2", _TIndex = 24}], _astChildren = []}
                                ]}
                            ]}
                        ],
                        [])
                actual `shouldBe` expected

            it "(let (type T [Number]) (var Number a 3) (print a))" $ do
                let actual = isLet $ generateTokens "(let (type T [Number]) (var Number a 3) (print a))"
                    expected = ([
                            AST_NODE {_astNodeType = AstLet, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstTypeAlias, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "T", _TIndex = 6}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstTypeDefinition, _astTokens = [], _astChildren = [
                                        AST_NODE {_astNodeType = AstListType, _astTokens = [], _astChildren = [
                                            AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "Number", _TIndex = 9}], _astChildren = []}
                                        ]}
                                    ]}
                                ]},

                                AST_NODE {_astNodeType = AstVar, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstTypeDefinition, _astTokens = [], _astChildren = [
                                        AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "Number", _TIndex = 16}], _astChildren = []}
                                    ]},
                                    AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "a", _TIndex = 18}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_Number, _TValue = "3", _TIndex = 20}], _astChildren = []}
                                ]},

                                AST_NODE {_astNodeType = AstFunctionCall, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "print", _TIndex = 24}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "a", _TIndex = 26}], _astChildren = []}
                                ]}
                            ]}
                        ],
                        [])
                actual `shouldBe` expected

            it "(let {f () 3} (print a))" $ do
                let actual = isLet $ generateTokens "(let {f () 3} (print a))"
                    expected = ([
                            AST_NODE {_astNodeType = AstLet, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstFunction, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "f", _TIndex = 4}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstParameterList, _astTokens = [], _astChildren = []},
                                    AST_NODE {_astNodeType = AstFunctionBody, _astTokens = [], _astChildren = [
                                        AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_Number, _TValue = "3", _TIndex = 9}], _astChildren = []}
                                    ]}
                                ]},
                                AST_NODE {_astNodeType = AstFunctionCall, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "print", _TIndex = 13}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "a", _TIndex = 15}], _astChildren = []}
                                ]}
                            ]}
                        ],
                        [])

                actual `shouldBe` expected
