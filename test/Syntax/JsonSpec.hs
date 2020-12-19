{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Syntax.JsonSpec where

    import Test.Hspec
    import Text.RawString.QQ
    import qualified Data.ByteString.Lazy.Char8 as L
    import Tokenizer
    import Syntax

    spec :: Spec
    spec = do
        describe "Syntax for JSON" $ do
            it "{ \"key\" 3 }" $ do
                let actual = isJson $ generateTokens "{ \"key\" 3 }"
                    expected = ([
                            AST_NODE {_astNodeType = AstJson, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstJsonKeyValue, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_String, _TValue = "\"key\"", _TIndex = 2}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_Number, _TValue = "3", _TIndex = 4}], _astChildren = []}
                                ]}
                            ]}
                        ],
                        [])
                actual `shouldBe` expected

            it "{ \"key\" \"value\" }" $ do
                let actual = isJson $ generateTokens "{ \"key\" \"value\" }"
                    expected = ([
                            AST_NODE {_astNodeType = AstJson, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstJsonKeyValue, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_String, _TValue = "\"key\"", _TIndex = 2}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_String, _TValue = "\"value\"", _TIndex = 4}], _astChildren = []}
                                ]}
                            ]}
                        ],
                        [])
                actual `shouldBe` expected

            it "{ \"key\" (f 1+3i) }" $ do
                let actual = isJson $ generateTokens "{ \"key\" (f 1+3i) }"
                    expected = ([
                            AST_NODE {_astNodeType = AstJson, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstJsonKeyValue, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_String, _TValue = "\"key\"", _TIndex = 2}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstFunctionCall, _astTokens = [], _astChildren = [
                                        AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "f", _TIndex = 5}], _astChildren = []},
                                        AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_ComplexNumber, _TValue = "1+3i", _TIndex = 7}], _astChildren = []}
                                    ]}
                                ]}
                            ]}
                        ],
                        [])
                actual `shouldBe` expected

            it "{ \"key\" { \"key2\" true } }" $ do
                let actual = isJson $ generateTokens "{ \"key\" { \"key2\" true } }"
                    expected = ([
                            AST_NODE {_astNodeType = AstJson, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstJsonKeyValue, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_String, _TValue = "\"key\"", _TIndex = 2}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstJson, _astTokens = [], _astChildren = [
                                        AST_NODE {_astNodeType = AstJsonKeyValue, _astTokens = [], _astChildren = [
                                            AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_String, _TValue = "\"key2\"", _TIndex = 6}], _astChildren = []},
                                            AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_BooleanTrue, _TValue = "true", _TIndex = 8}], _astChildren = []}
                                        ]}
                                    ]}
                                ]}
                            ]}
                        ],
                        [])
                actual `shouldBe` expected

            it "{ \"key\" [1 2 3] }" $ do
                let actual = isJson $ generateTokens "{ \"key\" [1 2 3] }"
                    expected = ([
                            AST_NODE {_astNodeType = AstJson, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstJsonKeyValue, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_String, _TValue = "\"key\"", _TIndex = 2}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstList, _astTokens = [], _astChildren = [
                                        AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_Number, _TValue = "1", _TIndex = 5}], _astChildren = []},
                                        AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_Number, _TValue = "2", _TIndex = 7}], _astChildren = []},
                                        AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_Number, _TValue = "3", _TIndex = 9}], _astChildren = []}
                                    ]}
                                ]}
                            ]}
                        ],
                        [])
                actual `shouldBe` expected

            it "{ \"key\" null }" $ do
                let actual = isJson $ generateTokens "{ \"key\" null }"
                    expected = ([
                            AST_NODE {_astNodeType = AstJson, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstJsonKeyValue, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_String, _TValue = "\"key\"", _TIndex = 2}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_JsonNull, _TValue = "null", _TIndex = 4}], _astChildren = []}
                                ]}
                            ]}
                        ],
                        [])
                actual `shouldBe` expected
