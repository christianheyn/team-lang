{-# LANGUAGE OverloadedStrings #-}

module Tokenizer.TokenizeSpec where

    import Test.Hspec
    import Text.RawString.QQ
    import qualified Data.ByteString.Lazy.Char8 as L
    import Tokenizer

    -- _TEST_T is just a shortcut to write a token quicker by hand
    _TEST_T t v i = Token { _TType = t, _TValue = v,  _TIndex = i }


    spec :: Spec
    spec = do
        describe "tokenize" $ do

            it "not" $ do
                let actual = tokenize "feature {t () ()}"
                    expected = ["feature"," ","{","t"," ","(",")"," ","(",")","}"]
                actual `shouldBe` expected
    -- spec :: Spec
    -- spec = do
    --     describe "tokenize" $ do

    --         it "x = -4.888" $ do
    --             let actual = tokenize "x = -4.888"
    --                 expected = ["x", " ", "=", " ", "-4.888"]
    --             actual `shouldBe` expected

    --         it "Number|String" $ do
    --             let actual = tokenize "Number|String"
    --                 expected = ["Number", "|", "String"]
    --             actual `shouldBe` expected


    --     describe "generateTokens" $ do

    --         it "x = -4.888" $ do
    --             let actual = generateTokens "x = -4.888"
    --                 expected = [
    --                       _TEST_T T_Symbol "x" 0
    --                     , _TEST_T T_Operator "=" 2
    --                     , _TEST_T T_Number "-4.888" 4
    --                     ]
    --             actual `shouldBe` expected

    --         it "0 + add(2)" $ do
    --             let actual = generateTokens "0 + add(2)"
    --                 expected = [
    --                       _TEST_T T_Number "0" 0
    --                     , _TEST_T T_Operator "+" 2
    --                     , _TEST_T T_Symbol "add" 4
    --                     , _TEST_T T_OpenRoundBracket "(" 5
    --                     , _TEST_T T_Number "2" 6
    --                     , _TEST_T T_ClosingRoundBracket ")" 7
    --                     ]
    --             actual `shouldBe` expected

    --         it "a(...all)" $ do
    --             let actual = generateTokens "a(...all)"
    --                 expected = [
    --                       _TEST_T T_Symbol "a" 0
    --                     , _TEST_T T_OpenRoundBracket "(" 1
    --                     , _TEST_T T_Operator "..." 2
    --                     , _TEST_T T_Symbol "all" 3
    --                     , _TEST_T T_ClosingRoundBracket ")" 4
    --                     ]
    --             actual `shouldBe` expected

    --         it "[1, 2] # comment {}" $ do
    --             let actual = generateTokens "[1, 2] # comment {}"
    --                 expected = [
    --                       _TEST_T T_OpenSquareBracket "[" 0
    --                     , _TEST_T T_Number "1" 1
    --                     , _TEST_T T_Separator "," 2
    --                     , _TEST_T T_Number "2" 4
    --                     , _TEST_T T_ClosingSquareBracket "]" 5
    --                     ]
    --             actual `shouldBe` expected


    --     describe "generateTokens (operator aliases)" $ do
    --         it "Number|String" $ do
    --             let actual = generateTokens "Number|String"
    --                 expected = [
    --                       _TEST_T T_Type "Number" 0
    --                     , _TEST_T T_Operator "|" 1
    --                     , _TEST_T T_Type "String" 2
    --                     ]
    --             actual `shouldBe` expected

    --         it "Number or String" $ do
    --             let actual = generateTokens "Number or String"
    --                 expected = [
    --                       _TEST_T T_Type "Number" 0
    --                     , _TEST_T T_Operator "or" 2
    --                     , _TEST_T T_Type "String" 4
    --                     ]
    --             actual `shouldBe` expected

    --         it "Number& String" $ do
    --             let actual = generateTokens "Number& String"
    --                 expected = [
    --                       _TEST_T T_Type "Number" 0
    --                     , _TEST_T T_Operator "&" 1
    --                     , _TEST_T T_Type "String" 3
    --                     ]
    --             actual `shouldBe` expected

    --         it "Number and String" $ do
    --             let actual = generateTokens "Number and String"
    --                 expected = [
    --                       _TEST_T T_Type "Number" 0
    --                     , _TEST_T T_Operator "and" 2
    --                     , _TEST_T T_Type "String" 4
    --                     ]
    --             actual `shouldBe` expected

    --         it "if(a = b)" $ do
    --             let actual = generateTokens "if(a = b)"
    --                 expected = [
    --                       _TEST_T T_ControlStrucKeyword "if" 0
    --                     , _TEST_T T_OpenRoundBracket "(" 1
    --                     , _TEST_T T_Symbol "a" 2
    --                     , _TEST_T T_Operator "=" 4
    --                     , _TEST_T T_Symbol "b" 6
    --                     , _TEST_T T_ClosingRoundBracket ")" 7
    --                     ]
    --             actual `shouldBe` expected

    --         it "if(a equals b)" $ do
    --             let actual = generateTokens "if(a equals b)"
    --                 expected = [
    --                       _TEST_T T_ControlStrucKeyword "if" 0
    --                     , _TEST_T T_OpenRoundBracket "(" 1
    --                     , _TEST_T T_Symbol "a" 2
    --                     , _TEST_T T_Operator "equals" 4
    --                     , _TEST_T T_Symbol "b" 6
    --                     , _TEST_T T_ClosingRoundBracket  ")" 7
    --                     ]
    --             actual `shouldBe` expected

    --         it "a != true" $ do
    --             let actual = generateTokens "a != true"
    --                 expected = [
    --                       _TEST_T T_Symbol "a" 0
    --                     , _TEST_T T_Operator "!=" 2
    --                     , _TEST_T T_BooleanConst "true" 4
    --                     ]
    --             actual `shouldBe` expected

    --         it "false equalsNot true" $ do
    --             let actual = generateTokens "false equalsNot true"
    --                 expected = [
    --                       _TEST_T T_BooleanConst "false" 0
    --                     , _TEST_T T_Operator "equalsNot" 2
    --                     , _TEST_T T_BooleanConst "true" 4
    --                     ]
    --             actual `shouldBe` expected

    --         it "f1 . f2" $ do
    --             let actual = generateTokens "f1 . f2"
    --                 expected = [
    --                       _TEST_T T_Symbol "f1" 0
    --                     , _TEST_T T_Operator "." 2
    --                     , _TEST_T T_Symbol "f2" 4
    --                     ]
    --             actual `shouldBe` expected

    --         it "f1 after f2" $ do
    --             let actual = generateTokens "f1 after f2"
    --                 expected = [
    --                       _TEST_T T_Symbol "f1" 0
    --                     , _TEST_T T_Operator "after" 2
    --                     , _TEST_T T_Symbol "f2" 4
    --                     ]
    --             actual `shouldBe` expected

    --         it "T -> T" $ do
    --             let actual = generateTokens "T -> T"
    --                 expected = [
    --                       _TEST_T T_Type "T" 0
    --                     , _TEST_T T_Operator "->" 2
    --                     , _TEST_T T_Type "T" 4
    --                     ]
    --             actual `shouldBe` expected

    --         it "T to T" $ do
    --             let actual = generateTokens "T to T"
    --                 expected = [
    --                       _TEST_T T_Type "T" 0
    --                     , _TEST_T T_Operator "to" 2
    --                     , _TEST_T T_Type "T" 4
    --                     ]
    --             actual `shouldBe` expected

    --         it "T => T" $ do
    --             let actual = generateTokens "T => T"
    --                 expected = [
    --                       _TEST_T T_Type "T" 0
    --                     , _TEST_T T_Operator "=>" 2
    --                     , _TEST_T T_Type "T" 4
    --                     ]
    --             actual `shouldBe` expected

    --         it "T yields T" $ do
    --             let actual = generateTokens "T yields T"
    --                 expected = [
    --                       _TEST_T T_Type "T" 0
    --                     , _TEST_T T_Operator "yields" 2
    --                     , _TEST_T T_Type "T" 4
    --                     ]
    --             actual `shouldBe` expected

    --         it "return a" $ do
    --             let actual = generateTokens "return a"
    --                 expected = [
    --                       _TEST_T T_Operator "return" 0
    --                     , _TEST_T T_Symbol "a" 2
    --                     ]
    --             actual `shouldBe` expected

    --         it "<- a" $ do
    --             let actual = generateTokens "<- a"
    --                 expected = [
    --                       _TEST_T T_Operator "<-" 0
    --                     , _TEST_T T_Symbol "a" 2
    --                     ]
    --             actual `shouldBe` expected

    --         it "2 > 3" $ do
    --             let actual = generateTokens "2 > 3"
    --                 expected = [
    --                       _TEST_T T_Number "2" 0
    --                     , _TEST_T T_Operator ">" 2
    --                     , _TEST_T T_Number "3" 4
    --                     ]
    --             actual `shouldBe` expected

    --         it "2 greaterThan 3" $ do
    --             let actual = generateTokens "2 greaterThan 3"
    --                 expected = [
    --                       _TEST_T T_Number "2" 0
    --                     , _TEST_T T_Operator "greaterThan" 2
    --                     , _TEST_T T_Number "3" 4
    --                     ]
    --             actual `shouldBe` expected

    --         it "2 < 3" $ do
    --             let actual = generateTokens "2 < 3"
    --                 expected = [
    --                       _TEST_T T_Number "2" 0
    --                     , _TEST_T T_Operator "<" 2
    --                     , _TEST_T T_Number "3" 4
    --                     ]
    --             actual `shouldBe` expected

    --         it "2 lessThan 3" $ do
    --             let actual = generateTokens "2 lessThan 3"
    --                 expected = [
    --                       _TEST_T T_Number "2" 0
    --                     , _TEST_T T_Operator "lessThan" 2
    --                     , _TEST_T T_Number "3" 4
    --                     ]
    --             actual `shouldBe` expected

    --         it "2 + 3" $ do
    --             let actual = generateTokens "2 + 3"
    --                 expected = [
    --                       _TEST_T T_Number "2" 0
    --                     , _TEST_T T_Operator "+" 2
    --                     , _TEST_T T_Number "3" 4
    --                     ]
    --             actual `shouldBe` expected

    --         it "2 plus 3" $ do
    --             let actual = generateTokens "2 plus 3"
    --                 expected = [
    --                       _TEST_T T_Number "2" 0
    --                     , _TEST_T T_Operator "plus" 2
    --                     , _TEST_T T_Number "3" 4
    --                     ]
    --             actual `shouldBe` expected

    --         it "2 - 3" $ do
    --             let actual = generateTokens "2 - 3"
    --                 expected = [
    --                       _TEST_T T_Number "2" 0
    --                     , _TEST_T T_Operator "-" 2
    --                     , _TEST_T T_Number "3" 4
    --                     ]
    --             actual `shouldBe` expected

    --         it "2 minus 3" $ do
    --             let actual = generateTokens "2 minus 3"
    --                 expected = [
    --                       _TEST_T T_Number "2" 0
    --                     , _TEST_T T_Operator "minus" 2
    --                     , _TEST_T T_Number "3" 4
    --                     ]
    --             actual `shouldBe` expected

    --         it "2 * 3" $ do
    --             let actual = generateTokens "2 * 3"
    --                 expected = [
    --                       _TEST_T T_Number "2" 0
    --                     , _TEST_T T_Operator "*" 2
    --                     , _TEST_T T_Number "3" 4
    --                     ]
    --             actual `shouldBe` expected

    --         it "2 times 3" $ do
    --             let actual = generateTokens "2 times 3"
    --                 expected = [
    --                       _TEST_T T_Number "2" 0
    --                     , _TEST_T T_Operator "times" 2
    --                     , _TEST_T T_Number "3" 4
    --                     ]
    --             actual `shouldBe` expected

    --         it "2 / 3" $ do
    --             let actual = generateTokens "2 / 3"
    --                 expected = [
    --                       _TEST_T T_Number "2" 0
    --                     , _TEST_T T_Operator "/" 2
    --                     , _TEST_T T_Number "3" 4
    --                     ]
    --             actual `shouldBe` expected

    --         it "2 div 3" $ do
    --             let actual = generateTokens "2 div 3"
    --                 expected = [
    --                       _TEST_T T_Number "2" 0
    --                     , _TEST_T T_Operator "div" 2
    --                     , _TEST_T T_Number "3" 4
    --                     ]
    --             actual `shouldBe` expected

    --     describe "generateTokens (strings)" $ do
    --         it "(\"str\", 3)" $ do
    --             let actual = generateTokens "(\"str\", 3)"
    --                 expected = [
    --                       _TEST_T T_OpenRoundBracket "(" 0
    --                     , _TEST_T T_String "\"str\"" 1
    --                     , _TEST_T T_Separator "," 2
    --                     , _TEST_T T_Number "3" 4
    --                     , _TEST_T T_ClosingRoundBracket ")" 5
    --                     ]
    --             actual `shouldBe` expected

    --         it "a = \"str\ntest\"" $ do
    --             let actual = generateTokens "a = \"str\ntest\""
    --                 expected = [
    --                       _TEST_T T_Symbol "a" 0
    --                     , _TEST_T T_Operator "=" 2
    --                     , _TEST_T T_String "\"str\ntest\"" 4
    --                     ]
    --             actual `shouldBe` expected

    --     describe "generateTokens (function flags)" $ do
    --         it "function a ::" $ do
    --             let actual = generateTokens "function a ::"
    --                 expected = [
    --                       _TEST_T T_FunctionFlagKeyword "function" 0
    --                     , _TEST_T T_Symbol "a" 2
    --                     , _TEST_T T_Operator "::" 4
    --                     ]
    --             actual `shouldBe` expected
