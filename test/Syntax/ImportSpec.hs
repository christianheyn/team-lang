{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Syntax.ImportSpec where

    import Test.Hspec
    import Text.RawString.QQ
    import qualified Data.ByteString.Lazy.Char8 as L
    import Tokenizer
    import Syntax

    spec :: Spec
    spec = do
        describe "Syntax for imports" $ do
            it "import (a b c) from \"./FeatureA.team\"" $ do
                let actual = isImport $ generateTokens "import (a b c) from \"./FeatureA.team\""
                    expected = ([
                            AST_NODE {_astNodeType = AstImport, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstOpen, _astTokens = [Token {_TType = T_OpenRoundBracket, _TValue = "(", _TIndex = 2}], _astChildren = []},
                                AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "a", _TIndex = 3}], _astChildren = []},
                                AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "b", _TIndex = 5}], _astChildren = []},
                                AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "c", _TIndex = 7}], _astChildren = []},
                                AST_NODE {_astNodeType = AstClose, _astTokens = [Token {_TType = T_ClosingRoundBracket, _TValue = ")", _TIndex = 8}], _astChildren = []},
                                AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_String, _TValue = "\"./FeatureA.team\"", _TIndex = 12}], _astChildren = []}
                            ]}
                        ],
                        [])
                actual `shouldBe` expected

            it "import (++ -- **) from \"./FeatureA.team\"" $ do
                let actual = isImport $ generateTokens "import (++ -- **) from \"./FeatureA.team\""
                    expected = ([
                            AST_NODE {_astNodeType = AstImport, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstOpen, _astTokens = [Token {_TType = T_OpenRoundBracket, _TValue = "(", _TIndex = 2}], _astChildren = []},
                                AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "++", _TIndex = 3}], _astChildren = []},
                                AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "--", _TIndex = 5}], _astChildren = []},
                                AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "**", _TIndex = 7}], _astChildren = []},
                                AST_NODE {_astNodeType = AstClose, _astTokens = [Token {_TType = T_ClosingRoundBracket, _TValue = ")", _TIndex = 8}], _astChildren = []},
                                AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_String, _TValue = "\"./FeatureA.team\"", _TIndex = 12}], _astChildren = []}
                            ]}
                        ],
                        [])
                actual `shouldBe` expected

            it "import (<++>) from \"./FeatureA.team\"" $ do
                let actual = isImport $ generateTokens "import (<++>) from \"./FeatureA.team\""
                    expected = ([
                            AST_NODE {_astNodeType = AstImport, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstOpen, _astTokens = [Token {_TType = T_OpenRoundBracket, _TValue = "(", _TIndex = 2}], _astChildren = []},
                                AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "<++>", _TIndex = 3}], _astChildren = []},
                                AST_NODE {_astNodeType = AstClose, _astTokens = [Token {_TType = T_ClosingRoundBracket, _TValue = ")", _TIndex = 4}], _astChildren = []},
                                AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_String, _TValue = "\"./FeatureA.team\"", _TIndex = 8}], _astChildren = []}
                            ]}
                        ],
                        [])
                actual `shouldBe` expected

            it "import as tdd from \"./FeatureA.team\"" $ do
                let actual = isImport $ generateTokens "import as tdd from \"./FeatureA.team\""
                    expected = ([
                            AST_NODE {_astNodeType = AstImportAs, _astTokens = [], _astChildren = [
                                AST_NODE {_astNodeType = AstSymbol, _astTokens = [Token {_TType = T_Symbol, _TValue = "tdd", _TIndex = 4}], _astChildren = []},
                                AST_NODE {_astNodeType = AstPrimitiv, _astTokens = [Token {_TType = T_String, _TValue = "\"./FeatureA.team\"", _TIndex = 8}], _astChildren = []}
                            ]}
                        ],
                        [])
                actual `shouldBe` expected
