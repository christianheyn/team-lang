{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Syntax.SwitchSpec where

    import Test.Hspec
    import Text.RawString.QQ
    import qualified Data.ByteString.Lazy.Char8 as L
    import Tokenizer
    import Syntax

    spec :: Spec
    spec = do
        describe "Syntax for switch" $ do

            let completeSwitch1 = [r|
    (switch x
        3 => false
        4 => true
        otherwise false)
            |]
            it completeSwitch1 $ do
                let actual = isSwitch $ generateTokens (L.pack completeSwitch1)
                    expected = ([
                            AST_NODE {_astNodeType = AstSwitch, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "x", _TIndex = 5}], _astChildren = []},
                                AST_NODE {_astNodeType = AstSwitchValue, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_Number, _TValue = "3", _TIndex = 8}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_BooleanFalse, _TValue = "false", _TIndex = 12}], _astChildren = []}
                                ]},
                                AST_NODE {_astNodeType = AstSwitchValue, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_Number, _TValue = "4", _TIndex = 15}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_BooleanTrue, _TValue = "true", _TIndex = 19}], _astChildren = []}
                                ]},
                                AST_NODE {_astNodeType = AstOtherwiseValue, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_BooleanFalse, _TValue = "false", _TIndex = 24}], _astChildren = []}
                                ]}
                            ]}
                        ],
                        [])
                actual `shouldBe` expected


            let completeSwitch2 = [r|
    switch x
        (3 => false)
        (4 => true)
        (otherwise false)
            |]
            it completeSwitch2 $ do
                let actual = isSwitch $ generateTokens (L.pack completeSwitch2)
                    expected = ([
                            AST_NODE {_astNodeType = AstSwitch, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "x", _TIndex = 4}], _astChildren = []},
                                AST_NODE {_astNodeType = AstSwitchValue, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_Number, _TValue = "3", _TIndex = 8}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_BooleanFalse, _TValue = "false", _TIndex = 12}], _astChildren = []}
                                ]},
                                AST_NODE {_astNodeType = AstSwitchValue, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_Number, _TValue = "4", _TIndex = 17}], _astChildren = []},
                                    AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_BooleanTrue, _TValue = "true", _TIndex = 21}], _astChildren = []}
                                ]},
                                AST_NODE {_astNodeType = AstOtherwiseValue, _astTokens = [], _astChildren = [
                                    AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_BooleanFalse, _TValue = "false", _TIndex = 28}], _astChildren = []}
                                ]}
                            ]}
                        ],
                        [])
                actual `shouldBe` expected
