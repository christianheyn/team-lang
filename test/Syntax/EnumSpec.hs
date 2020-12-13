{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Syntax.EnumSpec where

    import Test.Hspec
    import Text.RawString.QQ
    import qualified Data.ByteString.Lazy.Char8 as L
    import Tokenizer
    import Syntax

    spec :: Spec
    spec = do
        describe "Syntax for Enum" $ do
            it "(enum T :a)" $ do
                let actual = isEnum $ generateTokens "(enum T :a)"
                    expected = ([
                                AST_NODE {_astNodeType = AstEnum, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "T", _TIndex = 3}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstEnumMember, _astTokens = [Token {_TType = T_EnumMember, _TValue = ":a", _TIndex = 5}], _astChildren = []}
                                ]}
                            ],[])
                actual `shouldBe` expected

            it "T:a" $ do
                let actual = isEnumValue $ generateTokens "T:a"
                    expected = ([
                            AST_NODE {_astNodeType = AstEnumValue, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "T", _TIndex = 0}], _astChildren = []},
                                AST_NODE {_astNodeType = AstEnumMember, _astTokens = [Token {_TType = T_EnumMember, _TValue = ":a", _TIndex = 1}], _astChildren = []}
                            ]}
                        ],
                        [])
                actual `shouldBe` expected

            it "imported.T:a" $ do
                let actual = isEnumValue $ generateTokens "imported.T:a"
                    expected = ([
                            AST_NODE {_astNodeType = AstEnumValue, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstImportedTypeSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "imported", _TIndex = 0},Token {_TType = T_ReferenceDot, _TValue = ".", _TIndex = 1},Token {_TType = T_Type, _TValue = "T", _TIndex = 2}], _astChildren = []},
                                AST_NODE {_astNodeType = AstEnumMember, _astTokens = [Token {_TType = T_EnumMember, _TValue = ":a", _TIndex = 3}], _astChildren = []}
                            ]}
                        ],
                        [])
                actual `shouldBe` expected
