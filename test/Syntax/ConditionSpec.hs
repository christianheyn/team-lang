{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Syntax.ConditionSpec where

    import Test.Hspec
    import Text.RawString.QQ
    import qualified Data.ByteString.Lazy.Char8 as L
    import Tokenizer
    import Syntax

    spec :: Spec
    spec = do
        describe "Syntax for Conditions" $ do
            it "(if a then b else c)" $ do
                let actual = isIfThenElse $ generateTokens "(if a then b else c)"
                    expected = ([
                            AST_NODE {_astNodeType = AstIfCondition, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "a", _TIndex = 3}], _astChildren = []},
                                AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "b", _TIndex = 7}], _astChildren = []},
                                AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "c", _TIndex = 11}], _astChildren = []}
                            ]}
                        ],
                        [])
                actual `shouldBe` expected

            it "(if (< a 3) then (b) else (c))" $ do
                let actual = isIfThenElse $ generateTokens "(if (< a 3) then (b) else (c))"
                    expected = ([
                            AST_NODE {_astNodeType = AstIfCondition, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstFunctionCall, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "<", _TIndex = 4}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "a", _TIndex = 6}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_Number, _TValue = "3", _TIndex = 8}], _astChildren = []}]},
                                AST_NODE {_astNodeType = AstFunctionCall, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "b", _TIndex = 14}], _astChildren = []}]},
                                AST_NODE {_astNodeType = AstFunctionCall, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "c", _TIndex = 20}], _astChildren = []}
                                ]}
                            ]}
                        ],
                        [])
                actual `shouldBe` expected

            it "if (< a 3) then (b) else (c)" $ do
                let actual = isIfThenElse $ generateTokens "if (< a 3) then (b) else (c)"
                    expected = ([
                            AST_NODE {_astNodeType = AstIfCondition, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstFunctionCall, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "<", _TIndex = 3}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "a", _TIndex = 5}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_Number, _TValue = "3", _TIndex = 7}], _astChildren = []}]},
                                AST_NODE {_astNodeType = AstFunctionCall, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "b", _TIndex = 13}], _astChildren = []}]},
                                AST_NODE {_astNodeType = AstFunctionCall, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "c", _TIndex = 19}], _astChildren = []}
                                ]}
                            ]}
                        ],
                        [])
                actual `shouldBe` expected

            it "(if true then 3 else 5)" $ do
                let actual = isIfThenElse $ generateTokens "(if true then 3 else 5)"
                    expected = ([
                            AST_NODE {_astNodeType = AstIfCondition, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_BooleanTrue, _TValue = "true", _TIndex = 3}], _astChildren = []},
                                AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_Number, _TValue = "3", _TIndex = 7}], _astChildren = []},
                                AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_Number, _TValue = "5", _TIndex = 11}], _astChildren = []}
                            ]}
                        ],
                        [])
                actual `shouldBe` expected

            it "(if a then b else (if c then d else e))" $ do
                let actual = isIfThenElse $ generateTokens "(if a then b else (if c then d else e))"
                    expected = ([
                            AST_NODE {_astNodeType = AstIfCondition, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "a", _TIndex = 3}], _astChildren = []},
                                AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "b", _TIndex = 7}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstIfCondition, _astTokens = [], _astChildren = [
                                        AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "c", _TIndex = 14}], _astChildren = []},
                                        AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "d", _TIndex = 18}], _astChildren = []},
                                        AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_Number, _TValue = "e", _TIndex = 22}], _astChildren = []}
                                    ]}
                                ]}
                        ],
                        [])
                actual `shouldBe` expected
