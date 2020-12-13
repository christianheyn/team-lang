{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Syntax.TypesSpec where

    import Test.Hspec
    import Text.RawString.QQ
    import qualified Data.ByteString.Lazy.Char8 as L
    import Tokenizer
    import Syntax

    spec :: Spec
    spec = do
        describe "Syntax for types" $ do

            it "<T>" $ do
                let actual = isTemplateType $ generateTokens "<T>"
                    expected = ([AST_NODE {_astNodeType = AstTemplateType, _astTokens = [], _astChildren = [AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "T", _TIndex = 1}], _astChildren = []}]}],[])
                actual `shouldBe` expected

            -- it "CLASS: \"class Functor <T> { fmap  <U> {T -> U} -> [T] -> [U] }\"" $ do -- TODO: AstError check
            --     let actual = isClass $ generateTokens "class Functor <T> { fmap <U> {T -> U} -> [T] -> [U] }"
            --         expected = ([],[])
            --     actual `shouldBe` expected

            it "<U> {T -> U}" $ do
                let actual = isTypeDefinition $ generateTokens "<U> {T -> U}"
                    expected = ([
                        AST_NODE {_astNodeType = AstTypeDefinition, _astTokens = [], _astChildren = [
                            AST_NODE {_astNodeType = AstTemplateType, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "U", _TIndex = 1}], _astChildren = []}
                            ]},
                            AST_NODE {_astNodeType = AstFunctionType, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "T", _TIndex = 5}], _astChildren = []},
                                AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "U", _TIndex = 9}], _astChildren = []}
                            ]}
                        ]}
                        ],[])
                actual `shouldBe` expected

            it "<U> {T -> U} -> V" $ do
                let actual = isTypeDefinition $ generateTokens "<U> {T -> U} -> V"
                    expected = ([
                        AST_NODE {_astNodeType = AstTypeDefinition, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstTemplateType, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "U", _TIndex = 1}], _astChildren = []}]},
                            AST_NODE {_astNodeType = AstFunctionType, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "T", _TIndex = 5}], _astChildren = []},
                                AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "U", _TIndex = 9}], _astChildren = []}]},
                            AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "V", _TIndex = 14}], _astChildren = []}
                        ]}
                        ],
                        [])

                actual `shouldBe` expected

            it "[T] -> T" $ do
                let actual = isTypeDefinition $ generateTokens "[T] -> T"
                    expected = ([
                            AST_NODE {_astNodeType = AstTypeDefinition, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstListType, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "T", _TIndex = 1}], _astChildren = []}
                                ]},
                                AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "T", _TIndex = 6}], _astChildren = []}
                            ]}
                        ],[])
                actual `shouldBe` expected

            it "TYPEDEFINITION: \"T\"" $ do
                let actual = isTypeDefinition $ generateTokens "T"
                    expected = ([
                        AST_NODE {_astNodeType = AstTypeDefinition, _astTokens = [], _astChildren = [
                            AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "T", _TIndex = 0}], _astChildren = []}
                        ]}],[])
                actual `shouldBe` expected

            it "[T]" $ do
                let actual = isTypeDefinition $ generateTokens "[T]"
                    expected = ([
                        AST_NODE {_astNodeType = AstTypeDefinition, _astTokens = [], _astChildren = [
                            AST_NODE {_astNodeType = AstListType, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "T", _TIndex = 1}], _astChildren = []}
                            ]}
                        ]}
                        ],[])
                actual `shouldBe` expected

            it "[{T -> [U]}]" $ do
                let actual = isTypeDefinition $ generateTokens "[{T -> [U]}]"
                    expected = ([
                            AST_NODE {_astNodeType = AstTypeDefinition, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstListType, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstFunctionType, _astTokens = [], _astChildren = [
                                        AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "T", _TIndex = 2}], _astChildren = []},
                                        AST_NODE {_astNodeType = AstListType, _astTokens = [], _astChildren = [
                                            AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "U", _TIndex = 7}], _astChildren = []}
                                        ]}
                                    ]}
                                ]}
                            ]}
                        ],[])
                actual `shouldBe` expected

            it "class Functor <T> {}" $ do
                let actual = isClass $ generateTokens "class Functor <T> {}"
                    expected = ([
                        AST_NODE {_astNodeType = AstClass, _astTokens = [], _astChildren = [
                            AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "Functor", _TIndex = 2}], _astChildren = []},
                            AST_NODE {_astNodeType = AstTemplateType, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "T", _TIndex = 5}], _astChildren = []}
                            ]},
                            AST_NODE {_astNodeType = AstOpen, _astTokens = [Token {_TType = T_OpenCurlyBracket, _TValue = "{", _TIndex = 8}], _astChildren = []},
                            AST_NODE {_astNodeType = AstClose, _astTokens = [Token {_TType = T_ClosingCurlyBracket, _TValue = "}", _TIndex = 9}], _astChildren = []}
                        ]}
                        ],[])

                actual `shouldBe` expected

            let completeClass1 = [r|
    class Functor <T> {
        fmap <T> {T -> U} -> T -> U
        map {T -> U} -> T -> U
    }
            |]
            it (completeClass1 ++ "\n") $ do
                let actual = isClass $ generateTokens $ L.pack completeClass1
                    expected = ([
                            AST_NODE {_astNodeType = AstClass, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "Functor", _TIndex = 4}], _astChildren = []},
                                AST_NODE {_astNodeType = AstTemplateType, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "T", _TIndex = 7}], _astChildren = []}
                                ]},
                                AST_NODE {_astNodeType = AstOpen, _astTokens = [Token {_TType = T_OpenCurlyBracket, _TValue = "{", _TIndex = 10}], _astChildren = []},
                                AST_NODE {_astNodeType = AstClassFunction, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "fmap", _TIndex = 13}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstTypeDefinition, _astTokens = [], _astChildren = [
                                        AST_NODE {_astNodeType = AstTemplateType, _astTokens = [], _astChildren = [
                                            AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "T", _TIndex = 16}], _astChildren = []}
                                        ]},
                                        AST_NODE {_astNodeType = AstFunctionType, _astTokens = [], _astChildren = [
                                            AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "T", _TIndex = 20}], _astChildren = []},
                                            AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "U", _TIndex = 24}], _astChildren = []}
                                        ]},
                                        AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "T", _TIndex = 29}], _astChildren = []},
                                        AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "U", _TIndex = 33}], _astChildren = []}
                                    ]}
                                ]},
                                AST_NODE {_astNodeType = AstClassFunction, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "map", _TIndex = 36}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstTypeDefinition, _astTokens = [], _astChildren = [
                                        AST_NODE {_astNodeType = AstFunctionType, _astTokens = [], _astChildren = [
                                            AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "T", _TIndex = 39}], _astChildren = []},
                                            AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "U", _TIndex = 43}], _astChildren = []}
                                        ]},
                                        AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "T", _TIndex = 48}], _astChildren = []},
                                        AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "U", _TIndex = 52}], _astChildren = []}
                                    ]}
                                ]},
                                AST_NODE {_astNodeType = AstClose, _astTokens = [Token {_TType = T_ClosingCurlyBracket, _TValue = "}", _TIndex = 55}], _astChildren = []}
                            ]}
                        ],
                        [])
                actual `shouldBe` expected

            it "[a: Number b: Number -> Number]" $ do
                let actual = isPropListType $ generateTokens "[a: Number b: Number -> Number]"
                    expected = ([
                            AST_NODE {_astNodeType = AstPropListType, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstPropKeyValueType, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstProp, _astTokens = [Token {_TType = T_Prop, _TValue = "a:", _TIndex = 1}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstTypeDefinition, _astTokens = [], _astChildren = [
                                        AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "Number", _TIndex = 3}], _astChildren = []}
                                    ]}
                                ]},
                                AST_NODE {_astNodeType = AstPropKeyValueType, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstProp, _astTokens = [Token {_TType = T_Prop, _TValue = "b:", _TIndex = 5}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstTypeDefinition, _astTokens = [], _astChildren = [
                                        AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "Number", _TIndex = 7}], _astChildren = []},
                                        AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "Number", _TIndex = 11}], _astChildren = []}
                                    ]}
                                ]}
                            ]}
                        ],[])
                actual `shouldBe` expected

            it "<T> [a: T, b: Void] -> T" $ do
                let actual = isTypeDefinition $ generateTokens "<T> [a: T, b: Void] -> T"
                    expected = ([
                            AST_NODE {_astNodeType = AstTypeDefinition, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstTemplateType, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "T", _TIndex = 1}], _astChildren = []}
                                ]},
                                AST_NODE {_astNodeType = AstPropListType, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstPropKeyValueType, _astTokens = [], _astChildren = [
                                        AST_NODE {_astNodeType = AstProp, _astTokens = [Token {_TType = T_Prop, _TValue = "a:", _TIndex = 5}], _astChildren = []},
                                        AST_NODE {_astNodeType = AstTypeDefinition, _astTokens = [], _astChildren = [
                                            AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "T", _TIndex = 7}], _astChildren = []}
                                        ]}
                                    ]},
                                    AST_NODE {_astNodeType = AstPropKeyValueType, _astTokens = [], _astChildren = [
                                        AST_NODE {_astNodeType = AstProp, _astTokens = [Token {_TType = T_Prop, _TValue = "b:", _TIndex = 10}], _astChildren = []},
                                        AST_NODE {_astNodeType = AstTypeDefinition, _astTokens = [], _astChildren = [
                                            AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "Void", _TIndex = 12}], _astChildren = []}
                                        ]}
                                    ]}
                                ]},
                                AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "T", _TIndex = 17}], _astChildren = []}
                            ]}
                        ],[])
                actual `shouldBe` expected

            it "maybe T" $ do
                let actual = isTypeDefinition $ generateTokens "maybe T"
                    expected = ([
                            AST_NODE {_astNodeType = AstTypeDefinition, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstMaybeType, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "T", _TIndex = 2}], _astChildren = []}
                                ]}
                            ]}
                        ],[])
                actual `shouldBe` expected

            it "maybe maybe T" $ do
                let actual = isTypeDefinition $ generateTokens "maybe maybe T"
                    expected = ([
                            AST_NODE {_astNodeType = AstTypeDefinition, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstMaybeType, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstMaybeType, _astTokens = [], _astChildren = [
                                        AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "T", _TIndex = 4}], _astChildren = []}
                                    ]}
                                ]}
                            ]}
                        ],
                        [])
                actual `shouldBe` expected

            it "[a: [a: maybe [Number]]]" $ do
                let actual = isTypeDefinition $ generateTokens "[a: [a: maybe [Number]]]"
                    expected = ([
                            AST_NODE {_astNodeType = AstTypeDefinition, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstPropListType, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstPropKeyValueType, _astTokens = [], _astChildren = [
                                        AST_NODE {_astNodeType = AstProp, _astTokens = [Token {_TType = T_Prop, _TValue = "a:", _TIndex = 1}], _astChildren = []},
                                        AST_NODE {_astNodeType = AstTypeDefinition, _astTokens = [], _astChildren = [
                                            AST_NODE {_astNodeType = AstPropListType, _astTokens = [], _astChildren = [
                                                AST_NODE {_astNodeType = AstPropKeyValueType, _astTokens = [], _astChildren = [
                                                    AST_NODE {_astNodeType = AstProp, _astTokens = [Token {_TType = T_Prop, _TValue = "a:", _TIndex = 4}], _astChildren = []},
                                                    AST_NODE {_astNodeType = AstTypeDefinition, _astTokens = [], _astChildren = [
                                                        AST_NODE {_astNodeType = AstMaybeType, _astTokens = [], _astChildren = [
                                                            AST_NODE {_astNodeType = AstListType, _astTokens = [], _astChildren = [
                                                                AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "Number", _TIndex = 9}], _astChildren = []}
                                                            ]}
                                                        ]}
                                                    ]}
                                                ]}
                                            ]}
                                        ]}
                                    ]}
                                ]}
                            ]}
                        ],
                        [])

                actual `shouldBe` expected

            it "imported.Type -> imported.Type2" $ do
                let actual = isTypeDefinition $ generateTokens "imported.Type -> imported.Type2"
                    expected = ([
                            AST_NODE {_astNodeType = AstTypeDefinition, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstImportedTypeSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "imported", _TIndex = 0},Token {_TType = T_ReferenceDot, _TValue = ".", _TIndex = 1},Token {_TType = T_Type, _TValue = "Type", _TIndex = 2}], _astChildren = []},
                                AST_NODE {_astNodeType = AstImportedTypeSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "imported", _TIndex = 6},Token {_TType = T_ReferenceDot, _TValue = ".", _TIndex = 7},Token {_TType = T_Type, _TValue = "Type2", _TIndex = 8}], _astChildren = []}
                            ]}
                        ],
                        [])
                actual `shouldBe` expected

            it "T -> @U -> U" $ do
                let actual = isTypeDefinition $ generateTokens "T -> @U -> U"
                    expected = ([
                            AST_NODE {_astNodeType = AstTypeDefinition, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "T", _TIndex = 0}], _astChildren = []},
                                AST_NODE {_astNodeType = AstRestType, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "U", _TIndex = 5}], _astChildren = []}
                                ]},
                                AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "U", _TIndex = 9}], _astChildren = []}
                            ]}
                        ],
                        [])
                actual `shouldBe` expected

            it "T -> @imported.V -> U" $ do
                let actual = isTypeDefinition $ generateTokens "T -> @imported.V -> U"
                    expected = ([
                            AST_NODE {_astNodeType = AstTypeDefinition, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "T", _TIndex = 0}], _astChildren = []},
                                AST_NODE {_astNodeType = AstRestType, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstImportedTypeSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "imported", _TIndex = 5},Token {_TType = T_ReferenceDot, _TValue = ".", _TIndex = 6},Token {_TType = T_Type, _TValue = "V", _TIndex = 7}], _astChildren = []}]},
                                AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "U", _TIndex = 11}], _astChildren = []}
                            ]}
                        ],
                        [])
                actual `shouldBe` expected

            it "T -> @[a: U] -> U" $ do
                let actual = isTypeDefinition $ generateTokens "T -> @[a: U] -> U"
                    expected = ([
                            AST_NODE {_astNodeType = AstTypeDefinition, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "T", _TIndex = 0}], _astChildren = []},
                                AST_NODE {_astNodeType = AstRestType, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstPropListType, _astTokens = [], _astChildren = [
                                        AST_NODE {_astNodeType = AstPropKeyValueType, _astTokens = [], _astChildren = [
                                            AST_NODE {_astNodeType = AstProp, _astTokens = [Token {_TType = T_Prop, _TValue = "a:", _TIndex = 6}], _astChildren = []},
                                            AST_NODE {_astNodeType = AstTypeDefinition, _astTokens = [], _astChildren = [
                                                AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "U", _TIndex = 8}], _astChildren = []}
                                            ]}
                                        ]}
                                    ]}
                                ]},
                                AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "U", _TIndex = 13}], _astChildren = []}
                            ]}
                        ],[])
                actual `shouldBe` expected

            it "[a: maybe T -> Number b: Void]" $ do
                let actual = isTypeDefinition $ generateTokens "[a: maybe T -> Number b: Void]"
                    expected = ([
                            AST_NODE {_astNodeType = AstTypeDefinition, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstPropListType, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstPropKeyValueType, _astTokens = [], _astChildren = [
                                        AST_NODE {_astNodeType = AstProp, _astTokens = [Token {_TType = T_Prop, _TValue = "a:", _TIndex = 1}], _astChildren = []},
                                        AST_NODE {_astNodeType = AstTypeDefinition, _astTokens = [], _astChildren = [
                                            AST_NODE {_astNodeType = AstMaybeType, _astTokens = [], _astChildren = [
                                                AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "T", _TIndex = 5}], _astChildren = []}
                                            ]},
                                            AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "Number", _TIndex = 9}], _astChildren = []}
                                        ]}
                                    ]},
                                    AST_NODE {_astNodeType = AstPropKeyValueType, _astTokens = [], _astChildren = [
                                        AST_NODE {_astNodeType = AstProp, _astTokens = [Token {_TType = T_Prop, _TValue = "b:", _TIndex = 11}], _astChildren = []},
                                        AST_NODE {_astNodeType = AstTypeDefinition, _astTokens = [], _astChildren = [
                                            AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "Void", _TIndex = 13}], _astChildren = []}
                                        ]}
                                    ]}
                                ]}
                            ]}
                        ],
                        [])
                actual `shouldBe` expected

            it "ERROR: [a: <T> maybe T -> Number]" $ do
                let (nodes, _) = isTypeDefinition $ generateTokens "[a: <T> maybe T -> Number]"
                    actual = hasAstError nodes
                    expected = True
                actual `shouldBe` expected

            it "ERROR: @T" $ do
                let (nodes, restTokens) = isTypeDefinition $ generateTokens "@T"
                    actual = hasAstError nodes
                    expected = True
                restTokens `shouldBe` []

            it "ERROR(REST TOKENS): T -> @T -> T -> T" $ do
                let (nodes, restTokens) = isTypeDefinition $ generateTokens "T -> @T -> T -> T"
                    actual = length restTokens == 0
                    expected = False
                actual `shouldBe` expected

            it "@T -> T" $ do
                let (nodes, restTokens) = isTypeDefinition $ generateTokens "@T -> T"
                    actual = and [(not . hasAstError) nodes, length restTokens == 0]
                    expected = True
                actual `shouldBe` expected

            it "[a: imported.Type]" $ do
                let actual = isTypeDefinition $ generateTokens "[a: imported.Type]"
                    expected = ([
                            AST_NODE {_astNodeType = AstTypeDefinition, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstPropListType, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstPropKeyValueType, _astTokens = [], _astChildren = [
                                        AST_NODE {_astNodeType = AstProp, _astTokens = [Token {_TType = T_Prop, _TValue = "a:", _TIndex = 1}], _astChildren = []},
                                        AST_NODE {_astNodeType = AstTypeDefinition, _astTokens = [], _astChildren = [
                                            AST_NODE {_astNodeType = AstImportedTypeSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "imported", _TIndex = 3},Token {_TType = T_ReferenceDot, _TValue = ".", _TIndex = 4},Token {_TType = T_Type, _TValue = "Type", _TIndex = 5}], _astChildren = []}
                                        ]}
                                    ]}
                                ]}
                            ]}
                        ],
                        [])

                actual `shouldBe` expected

            it "{ \"key\" T }" $ do
                let actual = isJsonType $ generateTokens "{ \"key\" T }"
                    expected = ([
                            AST_NODE {_astNodeType = AstJsonType, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstJsonKeyValueType, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_String, _TValue = "\"key\"", _TIndex = 2}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "T", _TIndex = 4}], _astChildren = []}
                                ]}
                            ]}
                        ],
                        [])
                actual `shouldBe` expected

            it "{ \"key\" [T] }" $ do
                let actual = isJsonType $ generateTokens "{ \"key\" [T] }"
                    expected = ([
                            AST_NODE {_astNodeType = AstJsonType, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstJsonKeyValueType, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_String, _TValue = "\"key\"", _TIndex = 2}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstJsonArrayType, _astTokens = [], _astChildren = [
                                        AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "T", _TIndex = 5}], _astChildren = []}
                                    ]}
                                ]}
                            ]}
                        ],
                        [])
                actual `shouldBe` expected

            it "{ \"key\" { \"key2\" T } }" $ do
                let actual = isJsonType $ generateTokens "{ \"key\" { \"key2\" T } }"
                    expected = ([
                            AST_NODE {_astNodeType = AstJsonType, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstJsonKeyValueType, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_String, _TValue = "\"key\"", _TIndex = 2}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstJsonType, _astTokens = [], _astChildren = [
                                        AST_NODE {_astNodeType = AstJsonKeyValueType, _astTokens = [], _astChildren = [
                                            AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_String, _TValue = "\"key2\"", _TIndex = 6}], _astChildren = []},
                                            AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "T", _TIndex = 8}], _astChildren = []}
                                        ]}
                                    ]}
                                ]}
                            ]}
                        ],
                        [])
                actual `shouldBe` expected

            it "{ \"key\" [{ \"key2\" T }] }" $ do
                let actual = isJsonType $ generateTokens "{ \"key\" [{ \"key2\" T }] }"
                    expected = ([
                            AST_NODE {_astNodeType = AstJsonType, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstJsonKeyValueType, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_String, _TValue = "\"key\"", _TIndex = 2}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstJsonArrayType, _astTokens = [], _astChildren = [
                                        AST_NODE {_astNodeType = AstJsonType, _astTokens = [], _astChildren = [
                                            AST_NODE {_astNodeType = AstJsonKeyValueType, _astTokens = [], _astChildren = [
                                                AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_String, _TValue = "\"key2\"", _TIndex = 7}], _astChildren = []},
                                                AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "T", _TIndex = 9}], _astChildren = []}
                                            ]}
                                        ]}
                                    ]}
                                ]}
                            ]}
                        ],
                        [])
                actual `shouldBe` expected

            it "T -> { \"key\" [Number] }" $ do
                let actual = isTypeDefinition $ generateTokens "T -> { \"key\" [Number] }"
                    expected = ([
                            AST_NODE {_astNodeType = AstTypeDefinition, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "T", _TIndex = 0}], _astChildren = []},
                                AST_NODE {_astNodeType = AstJsonType, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstJsonKeyValueType, _astTokens = [], _astChildren = [
                                        AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_String, _TValue = "\"key\"", _TIndex = 6}], _astChildren = []},
                                        AST_NODE {_astNodeType = AstJsonArrayType, _astTokens = [], _astChildren = [
                                            AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "Number", _TIndex = 9}], _astChildren = []}
                                        ]}
                                    ]}
                                ]}
                            ]}
                        ],
                        [])
                actual `shouldBe` expected

            it "T -> { \"key\" [imported.U] }" $ do
                let actual = isTypeDefinition $ generateTokens "T -> { \"key\" [imported.U] }"
                    expected = ([
                            AST_NODE {_astNodeType = AstTypeDefinition, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "T", _TIndex = 0}], _astChildren = []},
                                AST_NODE {_astNodeType = AstJsonType, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstJsonKeyValueType, _astTokens = [], _astChildren = [
                                        AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_String, _TValue = "\"key\"", _TIndex = 6}], _astChildren = []},
                                        AST_NODE {_astNodeType = AstJsonArrayType, _astTokens = [], _astChildren = [
                                            AST_NODE {_astNodeType = AstImportedTypeSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "imported", _TIndex = 9},Token {_TType = T_ReferenceDot, _TValue = ".", _TIndex = 10},Token {_TType = T_Type, _TValue = "U", _TIndex = 11}], _astChildren = []}
                                        ]}
                                    ]}
                                ]}
                            ]}
                        ],
                        [])

                actual `shouldBe` expected