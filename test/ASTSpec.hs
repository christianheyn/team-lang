{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module ASTSpec where

    import Test.Hspec
    import Text.RawString.QQ
    import qualified Data.ByteString.Lazy.Char8 as L
    import Tokenizer
    import Syntax

    spec :: Spec
    spec = do
        describe "Syntax" $ do
            it "Parameter: \"(a, b)\"" $ do
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

            it "List: \"[1 [2 [3 true]]]\"" $ do
                let actual = isList $ generateTokens "[1 [2 [3 true]]]"
                    expected = (
                        [
                            AST_NODE {_astNodeType = AstList, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_Number, _TValue = "1", _TIndex = 1}], _astChildren = []},
                                AST_NODE {_astNodeType = AstList, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_Number, _TValue = "2", _TIndex = 4}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstList, _astTokens = [], _astChildren = [
                                        AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_Number, _TValue = "3", _TIndex = 7}], _astChildren = []},
                                        AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_BooleanTrue, _TValue = "true", _TIndex = 9}], _astChildren = []}
                                    ]}
                                ]}
                            ]}
                        ],
                        []
                        )
                actual `shouldBe` expected

            it "Function: \"{fn () 3}\"" $ do
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

            it "Function: \"{fn () a}\"" $ do
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

            it "Function: \"{fn (a) (+ a 1)}\"" $ do
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

            it "Lambda: \"{(a) (a)}\"" $ do
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

            it "Enum: \"(enum T :a)\"" $ do
                let actual = isEnum $ generateTokens "(enum T :a)"
                    expected = ([
                                AST_NODE {_astNodeType = AstEnum, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "T", _TIndex = 3}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstEnumMember, _astTokens = [Token {_TType = T_NamedParameter, _TValue = ":a", _TIndex = 5}], _astChildren = []}
                                ]}
                            ],[])
                actual `shouldBe` expected

            -- it "Enum: \"(enum T)\"" $ do -- TODO: AstError check
            --     let actual = isEnum $ generateTokens "(enum T)"
            --         expected = ([],[])
            --     actual `shouldBe` expected

            it "TEMPLATE TYPE: \"<T>\"" $ do
                let actual = isTemplateType $ generateTokens "<T>"
                    expected = ([AST_NODE {_astNodeType = AstTemplateType, _astTokens = [], _astChildren = [AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "T", _TIndex = 1}], _astChildren = []}]}],[])
                actual `shouldBe` expected

            -- it "CLASS: \"class Functor <T> { fmap  <U> {T -> U} -> [T] -> [U] }\"" $ do -- TODO: AstError check
            --     let actual = isClass $ generateTokens "class Functor <T> { fmap <U> {T -> U} -> [T] -> [U] }"
            --         expected = ([],[])
            --     actual `shouldBe` expected

            it "TYPEDEFINITION: \"<U> {T -> U}\"" $ do
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

            it "TYPEDEFINITION: \"<U> {T -> U} -> V\"" $ do
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

            it "TYPEDEFINITION: \"[T] -> T\"" $ do
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

            it "TYPEDEFINITION: \"[T]\"" $ do
                let actual = isTypeDefinition $ generateTokens "[T]"
                    expected = ([
                        AST_NODE {_astNodeType = AstTypeDefinition, _astTokens = [], _astChildren = [
                            AST_NODE {_astNodeType = AstListType, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "T", _TIndex = 1}], _astChildren = []}
                            ]}
                        ]}
                        ],[])
                actual `shouldBe` expected

            it "TYPEDEFINITION: \"[{T -> [U]}]\"" $ do
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

            it "CLASS: \"class Functor <T> {}\"" $ do
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

            it "CLASS: \"class Functor <T> { fmap <T> {T -> U} -> T -> U map  {T -> U} -> T -> U }\"" $ do
                let actual = isClass $ generateTokens "class Functor <T> { fmap <T> {T -> U} -> T -> U map  {T -> U} -> T -> U }"
                    expected = ([
                        AST_NODE {_astNodeType = AstClass, _astTokens = [], _astChildren = [
                            AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "Functor", _TIndex = 2}], _astChildren = []},
                            AST_NODE {_astNodeType = AstTemplateType, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "T", _TIndex = 5}], _astChildren = []}
                            ]},
                            AST_NODE {_astNodeType = AstOpen, _astTokens = [Token {_TType = T_OpenCurlyBracket, _TValue = "{", _TIndex = 8}], _astChildren = []},
                            AST_NODE {_astNodeType = AstClassFunction, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "fmap", _TIndex = 10}], _astChildren = []},
                                AST_NODE {_astNodeType = AstTypeDefinition, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstTemplateType, _astTokens = [], _astChildren = [
                                        AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "T", _TIndex = 13}], _astChildren = []}
                                    ]},
                                    AST_NODE {_astNodeType = AstFunctionType, _astTokens = [], _astChildren = [
                                        AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "T", _TIndex = 17}], _astChildren = []},
                                        AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "U", _TIndex = 21}], _astChildren = []}
                                    ]},
                                    AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "T", _TIndex = 26}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "U", _TIndex = 30}], _astChildren = []}
                                ]}
                            ]},
                            AST_NODE {_astNodeType = AstClassFunction, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "map", _TIndex = 32}], _astChildren = []},
                                AST_NODE {_astNodeType = AstTypeDefinition, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstFunctionType, _astTokens = [], _astChildren = [
                                        AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "T", _TIndex = 35}], _astChildren = []},
                                        AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "U", _TIndex = 39}], _astChildren = []}]},
                                    AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "T", _TIndex = 44}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "U", _TIndex = 48}], _astChildren = []}
                                ]}
                            ]},
                            AST_NODE {_astNodeType = AstClose, _astTokens = [Token {_TType = T_ClosingCurlyBracket, _TValue = "}", _TIndex = 50}], _astChildren = []}]
                        }],
                        [])
                actual `shouldBe` expected

            it "PROPLIST: \"[a: Number b: Number -> Number]\"" $ do
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

            it "TYPEDEFINITION: \"<T> [a: T, b: Void] -> T\"" $ do
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

            it "TYPEDEFINITION: \"maybe T\"" $ do
                let actual = isTypeDefinition $ generateTokens "maybe T"
                    expected = ([
                            AST_NODE {_astNodeType = AstTypeDefinition, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstMaybeType, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "T", _TIndex = 2}], _astChildren = []}
                                ]}
                            ]}
                        ],[])
                actual `shouldBe` expected

            it "TYPEDEFINITION: \"maybe maybe T\"" $ do
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

            it "TYPEDEFINITION: \"[a: [a: maybe [Number]]]\"" $ do
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

            it "TYPEDEFINITION: \"imported.Type -> imported.Type2\"" $ do
                let actual = isTypeDefinition $ generateTokens "imported.Type -> imported.Type2"
                    expected = ([
                            AST_NODE {_astNodeType = AstTypeDefinition, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstImportedTypeSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "imported", _TIndex = 0},Token {_TType = T_ReferenceDot, _TValue = ".", _TIndex = 1},Token {_TType = T_Type, _TValue = "Type", _TIndex = 2}], _astChildren = []},
                                AST_NODE {_astNodeType = AstImportedTypeSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "imported", _TIndex = 6},Token {_TType = T_ReferenceDot, _TValue = ".", _TIndex = 7},Token {_TType = T_Type, _TValue = "Type2", _TIndex = 8}], _astChildren = []}
                            ]}
                        ],
                        [])
                actual `shouldBe` expected

            it "TYPEDEFINITION REST: \"T -> @U -> U\"" $ do
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

            it "TYPEDEFINITION REST: \"T -> @imported.V -> U\"" $ do
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

            it "TYPEDEFINITION REST: \"T -> @[a: U] -> U\"" $ do
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

            it "TYPEDEFINITION: \"[a: maybe T -> Number b: Void]\"" $ do
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

            it "ERROR: TYPEDEFINITION: \"[a: <T> maybe T -> Number]\"" $ do
                let (nodes, _) = isTypeDefinition $ generateTokens "[a: <T> maybe T -> Number]"
                    actual = hasAstError nodes
                    expected = True
                actual `shouldBe` expected

            it "ERROR: TYPEDEFINITION: \"@T\"" $ do
                let (nodes, restTokens) = isTypeDefinition $ generateTokens "@T"
                    actual = hasAstError nodes
                    expected = True
                restTokens `shouldBe` []

            it "ERROR(REST TOKENS): TYPEDEFINITION: \"T -> @T -> T -> T\"" $ do
                let (nodes, restTokens) = isTypeDefinition $ generateTokens "T -> @T -> T -> T"
                    actual = length restTokens == 0
                    expected = False
                actual `shouldBe` expected

            it "TYPEDEFINITION: \"@T -> T\"" $ do
                let (nodes, restTokens) = isTypeDefinition $ generateTokens "@T -> T"
                    actual = and [(not . hasAstError) nodes, length restTokens == 0]
                    expected = True
                actual `shouldBe` expected

            it "TYPEDEFINITION: \"[a: imported.Type]\"" $ do
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


            let completeFunction1 = [r|
    {f
        <T> Number -> T -> imported.User -> T
        (n t user)
        (wip)
    }
            |]
            it ("FUNCTION: \"" ++ completeFunction1 ++ "\n\"") $ do
                let actual = isFunction $ generateTokens $ L.pack completeFunction1
                    expected = ([
                            AST_NODE {_astNodeType = AstFunction, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "f", _TIndex = 3}], _astChildren = []},
                                AST_NODE {_astNodeType = AstTypeDefinition, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstTemplateType, _astTokens = [], _astChildren = [
                                        AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "T", _TIndex = 7}], _astChildren = []}]},
                                    AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "Number", _TIndex = 10}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "T", _TIndex = 14}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstImportedTypeSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "imported", _TIndex = 18},Token {_TType = T_ReferenceDot, _TValue = ".", _TIndex = 19},Token {_TType = T_Type, _TValue = "User", _TIndex = 20}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstTypeSymbol, _astTokens = [Token {_TType = T_Type, _TValue = "T", _TIndex = 24}], _astChildren = []}]},
                                AST_NODE {_astNodeType = AstParameterList, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstParameter, _astTokens = [Token {_TType = T_Symbol, _TValue = "n", _TIndex = 28}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstParameter, _astTokens = [Token {_TType = T_Symbol, _TValue = "t", _TIndex = 30}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstParameter, _astTokens = [Token {_TType = T_Symbol, _TValue = "user", _TIndex = 32}], _astChildren = []}]},
                                AST_NODE {_astNodeType = AstFunctionBody, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstFunctionCall, _astTokens = [], _astChildren = [
                                        AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "wip", _TIndex = 37}], _astChildren = []}
                                    ]}
                                ]}
                            ]}
                        ],
                        [])
                actual `shouldBe` expected


