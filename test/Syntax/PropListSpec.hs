{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Syntax.PropListSpec where

    import Test.Hspec
    import Text.RawString.QQ
    import qualified Data.ByteString.Lazy.Char8 as L
    import Tokenizer
    import Syntax

    spec :: Spec
    spec = do
        describe "Syntax for PropList" $ do
            it "[a: 3 b: \"text\"]" $ do
                let actual = isPropList $ generateTokens "[a: 3 b: \"text\"]"
                    expected = ([
                            AST_NODE {_astNodeType = AstPropList, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstPropKeyValue, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstProp, _astTokens = [Token {_TType = T_Prop, _TValue = "a:", _TIndex = 1}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_Number, _TValue = "3", _TIndex = 3}], _astChildren = []}
                                ]},
                                AST_NODE {_astNodeType = AstPropKeyValue, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstProp, _astTokens = [Token {_TType = T_Prop, _TValue = "b:", _TIndex = 5}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_String, _TValue = "\"text\"", _TIndex = 7}], _astChildren = []}
                                ]}
                            ]}
                        ],
                        [])
                actual `shouldBe` expected

            it "[a: 3 b: [a: 3 b: \"text\"]]" $ do
                let actual = isPropList $ generateTokens "[a: 3 b: [a: 3 b: \"text\"]]"
                    expected = ([
                            AST_NODE {_astNodeType = AstPropList, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstPropKeyValue, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstProp, _astTokens = [Token {_TType = T_Prop, _TValue = "a:", _TIndex = 1}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_Number, _TValue = "3", _TIndex = 3}], _astChildren = []}
                                ]},
                                AST_NODE {_astNodeType = AstPropKeyValue, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstProp, _astTokens = [Token {_TType = T_Prop, _TValue = "b:", _TIndex = 5}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstPropList, _astTokens = [], _astChildren = [
                                        AST_NODE {_astNodeType = AstPropKeyValue, _astTokens = [], _astChildren = [
                                            AST_NODE {_astNodeType = AstProp, _astTokens = [Token {_TType = T_Prop, _TValue = "a:", _TIndex = 8}], _astChildren = []},
                                            AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_Number, _TValue = "3", _TIndex = 10}], _astChildren = []}
                                        ]},
                                        AST_NODE {_astNodeType = AstPropKeyValue, _astTokens = [], _astChildren = [
                                            AST_NODE {_astNodeType = AstProp, _astTokens = [Token {_TType = T_Prop, _TValue = "b:", _TIndex = 12}], _astChildren = []},
                                            AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_String, _TValue = "\"text\"", _TIndex = 14}], _astChildren = []}
                                        ]}
                                    ]}
                                ]}
                            ]}
                        ],
                        [])

                actual `shouldBe` expected

            it "[a: {(x) void}]" $ do
                let actual = isPropList $ generateTokens "[a: {(x) void}]"
                    expected = ([
                            AST_NODE {_astNodeType = AstPropList, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstPropKeyValue, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstProp, _astTokens = [Token {_TType = T_Prop, _TValue = "a:", _TIndex = 1}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstLambda, _astTokens = [], _astChildren = [
                                        AST_NODE {_astNodeType = AstParameterList, _astTokens = [], _astChildren = [
                                            AST_NODE {_astNodeType = AstParameter, _astTokens = [Token {_TType = T_Symbol, _TValue = "x", _TIndex = 5}], _astChildren = []}
                                        ]},
                                        AST_NODE {_astNodeType = AstFunctionBody, _astTokens = [], _astChildren = [
                                            AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_Void, _TValue = "void", _TIndex = 8}], _astChildren = []}
                                        ]}
                                    ]}
                                ]}
                            ]}
                        ],
                        [])
                actual `shouldBe` expected

