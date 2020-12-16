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
            it "var a Number = 3" $ do
                let actual = isVar $ generateTokens "var a Number = 3"
                    expected = ([
                            AST_NODE {_astNodeType = AstVar, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "a", _TIndex = 2}], _astChildren = []},
                                AST_NODE {_astNodeType = AstTypeDefinition, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "Number", _TIndex = 4}], _astChildren = []}
                                ]},
                                AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_Number, _TValue = "3", _TIndex = 8}], _astChildren = []}
                            ]}
                        ],
                        [])
                actual `shouldBe` expected

            it "a Number = 3" $ do
                let actual = isVar $ generateTokens "a Number = 3"
                    expected = ([
                            AST_NODE {_astNodeType = AstVar, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "a", _TIndex = 0}], _astChildren = []},
                                AST_NODE {_astNodeType = AstTypeDefinition, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "Number", _TIndex = 2}], _astChildren = []}
                                ]},
                                AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_Number, _TValue = "3", _TIndex = 6}], _astChildren = []}
                            ]}
                        ],
                        [])
                actual `shouldBe` expected

            it "var a b c Number = 3" $ do
                let actual = isVar $ generateTokens "var a b c Number = 3"
                    expected = ([
                            AST_NODE {_astNodeType = AstVar, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "a", _TIndex = 2}], _astChildren = []},
                                AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "b", _TIndex = 4}], _astChildren = []},
                                AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "c", _TIndex = 6}], _astChildren = []},
                                AST_NODE {_astNodeType = AstTypeDefinition, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "Number", _TIndex = 8}], _astChildren = []}
                                ]},
                                AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_Number, _TValue = "3", _TIndex = 12}], _astChildren = []}
                            ]}
                        ],
                        [])
                actual `shouldBe` expected

            it "a b c Number = 3" $ do
                let actual = isVar $ generateTokens "a b c Number = 3"
                    expected = ([
                            AST_NODE {_astNodeType = AstVar, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "a", _TIndex = 0}], _astChildren = []},
                                AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "b", _TIndex = 2}], _astChildren = []},
                                AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "c", _TIndex = 4}], _astChildren = []},
                                AST_NODE {_astNodeType = AstTypeDefinition, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "Number", _TIndex = 6}], _astChildren = []}
                                ]},
                                AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_Number, _TValue = "3", _TIndex = 10}], _astChildren = []}
                            ]}
                        ],
                        [])
                actual `shouldBe` expected

            it "var a maybe Number = void" $ do
                let actual = isVar $ generateTokens "var a maybe Number = void"
                    expected = ([
                            AST_NODE {_astNodeType = AstVar, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "a", _TIndex = 2}], _astChildren = []},
                                AST_NODE {_astNodeType = AstTypeDefinition, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstMaybeType, _astTokens = [], _astChildren = [
                                        AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "Number", _TIndex = 6}], _astChildren = []}
                                    ]}
                                ]},
                                AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_Void, _TValue = "void", _TIndex = 10}], _astChildren = []}
                            ]}
                        ],
                        [])
                actual `shouldBe` expected

            it "a maybe Number = void" $ do
                let actual = isVar $ generateTokens "a maybe Number = void"
                    expected = ([
                            AST_NODE {_astNodeType = AstVar, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "a", _TIndex = 0}], _astChildren = []},
                                AST_NODE {_astNodeType = AstTypeDefinition, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstMaybeType, _astTokens = [], _astChildren = [
                                        AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "Number", _TIndex = 4}], _astChildren = []}
                                    ]}
                                ]},
                                AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_Void, _TValue = "void", _TIndex = 8}], _astChildren = []}
                            ]}
                        ],
                        [])
                actual `shouldBe` expected

            it "var a Number = (if (< b 3) then 4 else 2)" $ do
                let actual = isVar $ generateTokens "var a Number = (if (< b 3) then 4 else 2)"
                    expected = ([
                            AST_NODE {_astNodeType = AstVar, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "a", _TIndex = 2}], _astChildren = []},
                                AST_NODE {_astNodeType = AstTypeDefinition, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "Number", _TIndex = 4}], _astChildren = []}]},
                                AST_NODE {_astNodeType = AstIfCondition, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstFunctionCall, _astTokens = [], _astChildren = [
                                        AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "<", _TIndex = 12}], _astChildren = []},
                                        AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "b", _TIndex = 14}], _astChildren = []},
                                        AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_Number, _TValue = "3", _TIndex = 16}], _astChildren = []}]},
                                    AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_Number, _TValue = "4", _TIndex = 21}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_Number, _TValue = "2", _TIndex = 25}], _astChildren = []}
                                ]}
                            ]}
                        ],
                        [])
                actual `shouldBe` expected

            it "(let type T = [Number]; var a Number = 3; (print a))" $ do
                let actual = isLet $ generateTokens "(let type T = [Number]; var a Number = 3; (print a))"
                    expected = ([
                            AST_NODE {_astNodeType = AstLet, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstTypeAlias, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "T", _TIndex = 5}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstTypeDefinition, _astTokens = [], _astChildren = [
                                        AST_NODE {_astNodeType = AstListType, _astTokens = [], _astChildren = [
                                            AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "Number", _TIndex = 10}], _astChildren = []}
                                        ]}
                                    ]}
                                ]},
                                AST_NODE {_astNodeType = AstVar, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "a", _TIndex = 16}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstTypeDefinition, _astTokens = [], _astChildren = [
                                        AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "Number", _TIndex = 18}], _astChildren = []}
                                    ]},
                                    AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_Number, _TValue = "3", _TIndex = 22}], _astChildren = []}
                                ]},
                                AST_NODE {_astNodeType = AstFunctionCall, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "print", _TIndex = 26}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "a", _TIndex = 28}], _astChildren = []}
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

            it "(let a = 3 T = [Number] b c d = \"text\" (print c))" $ do
                let actual = isLet $ generateTokens "(let a = 3 T = [Number] b c d = \"text\" (print c))"
                    expected = ([
                            AST_NODE {_astNodeType = AstLet, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstVar, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "a", _TIndex = 3}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_Number, _TValue = "3", _TIndex = 7}], _astChildren = []}
                                ]},
                                AST_NODE {_astNodeType = AstTypeAlias, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "T", _TIndex = 9}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstTypeDefinition, _astTokens = [], _astChildren = [
                                        AST_NODE {_astNodeType = AstListType, _astTokens = [], _astChildren = [
                                            AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "Number", _TIndex = 14}], _astChildren = []}
                                        ]}
                                    ]}
                                ]},
                                AST_NODE {_astNodeType = AstVar, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "b", _TIndex = 17}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "c", _TIndex = 19}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "d", _TIndex = 21}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_String, _TValue = "\"text\"", _TIndex = 25}], _astChildren = []}
                                ]},
                                AST_NODE {_astNodeType = AstFunctionCall, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "print", _TIndex = 28}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "c", _TIndex = 30}], _astChildren = []}
                                ]}
                            ]}
                        ],
                        [])

                actual `shouldBe` expected
