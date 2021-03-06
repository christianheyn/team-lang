{-# LANGUAGE OverloadedStrings #-}

module Tokenizer.RegexSpec where

    import Test.Hspec
    import Text.RawString.QQ
    import qualified Data.ByteString.Lazy.Char8 as L
    import Tokenizer

    checkSymbol s = do
        it s $ do
            let actual = _isSymbol $ L.pack s
                expected = True
            actual `shouldBe` expected

    checkNotSymbol x y = do
        it ("NOT: " ++ x) $ do
            let actual = _isSymbol $ y
                expected = False
            actual `shouldBe` expected

    checkProp p = do
        it p $ do
            let actual = _isProp $ L.pack p
                expected = True
            actual `shouldBe` expected

    checkEnumMember p = do
        it p $ do
            let actual = _isEnumMember $ L.pack p
                expected = True
            actual `shouldBe` expected

    checkType t = do
        it t $ do
            let actual = _isType $ L.pack t
                expected = True
            actual `shouldBe` expected

    checkUnresolvedNumber n = do
        it n $ do
            let actual = _isUnresolvedNumber $ L.pack n
                expected = True
            actual `shouldBe` expected

    checkNotUnresolvedNumber n m = do
        it ("NOT: " ++ n) $ do
            let actual = _isUnresolvedNumber m
                expected = False
            actual `shouldBe` expected

    checkNumber n = do
        it n $ do
            let actual = _isNumber $ L.pack n
                expected = True
            actual `shouldBe` expected

    checkUnresolvedComplexNumber n = do
        it n $ do
            let actual = _isUnresolvedComplexNumber $ L.pack n
                expected = True
            actual `shouldBe` expected

    checkNotUnresolvedComplexNumber n m = do
        it ("NOT: " ++ n) $ do
            let actual = _isUnresolvedComplexNumber m
                expected = False
            actual `shouldBe` expected

    checkComplexNumber n = do
        it n $ do
            let actual = _isComplexNumber $ L.pack n
                expected = True
            actual `shouldBe` expected

    checkNotNumber n m = do
        it ("NOT: " ++ n) $ do
            let actual = _isNumber m
                expected = False
            actual `shouldBe` expected

    checkUnresolvedString s = do
        it s $ do
            let actual = _isUnresolvedString $ L.pack s
                expected = True
            actual `shouldBe` expected

    checkNotUnresolvedString s s' = do
        it ("NOT: " ++ s) $ do
            let actual = _isUnresolvedString s'
                expected = False
            actual `shouldBe` expected

    checkString s = do
        it s $ do
            let actual = _isString $ L.pack s
                expected = True
            actual `shouldBe` expected

    checkNotString s s' = do
        it ("NOT: " ++ s) $ do
            let actual = _isString s'
                expected = False
            actual `shouldBe` expected

    spec :: Spec
    spec = do
        describe "Token Regex" $ do
            describe "_isReferenceDot" $ do
                it "." $ do
                    let actual = _isReferenceDot "."
                        expected = True
                    actual `shouldBe` expected

                it "NOT: .." $ do
                    let actual = _isReferenceDot ".."
                        expected = False
                    actual `shouldBe` expected

            describe "_isRestSpread" $ do
                it "@" $ do
                    let actual = _isRestSpread "@"
                        expected = True
                    actual `shouldBe` expected

            describe "_isSymbol" $ do
                checkSymbol "a"
                checkSymbol "a'"
                checkSymbol "a''"
                checkSymbol "a'''"
                checkSymbol "aA"
                checkSymbol "_23"
                checkSymbol "+"
                checkSymbol "-"
                checkSymbol "*"
                checkSymbol "/"
                checkSymbol "="
                checkSymbol "!"
                checkSymbol "?"
                checkSymbol "$"
                checkSymbol "<"
                checkSymbol ">"
                checkSymbol "~"
                checkSymbol "_"
                checkSymbol "%"
                checkSymbol "|"
                checkSymbol "&"
                checkSymbol "<>"
                checkSymbol "abc<$$$>"
                checkSymbol "-aBcD123"
                checkSymbol "^"
                checkSymbol "^^"

                checkNotSymbol "<T>" "<T>"
                checkNotSymbol "_23+-!$></Aa****%" "_23+-!$></Aa****%"
                checkNotSymbol "\\naaa" "\naaa"
                checkNotSymbol ":" ":"
                checkNotSymbol "a:" "a:"
                checkNotSymbol "_123:" "_123:"
                checkNotSymbol "a-b:" "a-b:"

            describe "_isProp" $ do
                checkProp "a:"
                checkProp "_123:"
                checkProp "a-b:"
                checkProp "a-2:"

            describe "_isEnumMember" $ do
                checkEnumMember ":a"
                checkEnumMember ":_123"
                checkEnumMember ":a-b"
                checkEnumMember ":a-2"
                checkEnumMember ":1"

            describe "_isType" $ do
                checkType "A"
                checkType "Z"
                checkType "Abc"
                checkType "Z_a"
                checkType "Z012_Ax"
                checkType "Z___"

            describe "_isUnresolvedNumber" $ do
                checkUnresolvedNumber "1."
                checkUnresolvedNumber "0."
                checkUnresolvedNumber "23."
                checkUnresolvedNumber "2/"
                checkNotUnresolvedNumber "00" "00"
                checkNotUnresolvedNumber "00." "00."
                checkNotUnresolvedNumber "023." "023."
                checkNotUnresolvedNumber "07/" "07/"
                checkNotUnresolvedNumber "0.7/" "0.7/"
                checkNotUnresolvedNumber "7/09" "7/09"
                checkNotUnresolvedNumber "7/09.5" "7/09.5"
                checkNotUnresolvedNumber "7/09.5" "7/09.5"

            describe "_isNumber" $ do
                checkNumber "0"
                checkNumber "23"
                checkNumber "2321376197319827538125347126543615234"
                checkNumber "0.0"
                checkNumber "0.2"
                checkNumber "121231230.89712098376912870"
                checkNumber "-0"
                checkNumber "-0.0"
                checkNumber "-3"
                checkNumber "-0.2"
                checkNumber "-121231230.89712098376912870"
                checkNumber "2/5"
                checkNumber "4/7115"
                checkNumber "40/70"
                checkNumber "-40/+70"
                checkNumber "-40/-70"
                checkNumber "1/-3"

                checkNotNumber "00.0" "00.0"
                checkNotNumber "0123" "0123"
                checkNotNumber "0..0" "0..0"
                checkNotNumber "0.12.3" "0.12.3"
                checkNotNumber "0." "0."
                checkNotNumber "123." "123."
                checkNotNumber "0/0" "0/0"
                checkNotNumber "2/0" "2/0"
                checkNotNumber "0/3" "0/3"
                checkNotNumber "0.4/3" "0.4/3"
                checkNotNumber "2/3.5" "2/3.5"

            describe "_isUnresolvedComplexNumber" $ do
                checkUnresolvedComplexNumber "3+"
                checkUnresolvedComplexNumber "3+4"
                checkUnresolvedComplexNumber "-3+-4"
                checkUnresolvedComplexNumber "-3+-"
                checkUnresolvedComplexNumber "-3+-"
                checkUnresolvedComplexNumber "-3+-2."

            describe "_isComplexNumber" $ do
                checkComplexNumber "-2+3i"
                checkComplexNumber "+2+-3i"
                checkComplexNumber "-2.0+-3.4i"
                checkComplexNumber "20+3.000i"

            describe "_endingChars" $ do
                it "test\"" $ do
                    let actual = _endingChars '"' "test\""
                        expected = 1
                    actual `shouldBe` expected

                it "test\\\"" $ do
                    let actual = _endingChars '"' "test\\\""
                        expected = 0
                    actual `shouldBe` expected

            describe "_isUnresolvedString" $ do
                checkUnresolvedString "\"test"
                checkUnresolvedString "\"test\\\""
                checkNotUnresolvedString "\"test\\\"\"" "\"test\\\"\""
                checkNotUnresolvedString "\"test\\\"\"a" "\"test\\\"\"a"

            describe "_isString" $ do
                checkString "\"\""
                checkString "\"test\""
                checkString "\"test\ntest\""
                checkString "\"test\ntest\""
                checkString "\"attr=\\\"value\\\"\""

                checkNotString "test" "test"
                checkNotString "test\"" "test\""
                checkNotString "\"test\\\"" "\"test\\\""
                checkNotString "\"test\"a" "\"test\"a"
                checkNotString "\"test\ntest\"a" "\"test\ntest\"a"

            describe "_isComment" $ do
                it "# " $ do
                    let actual = _isComment "# "
                        expected = True
                    actual `shouldBe` expected

                it "# comment" $ do
                    let actual = _isComment "# comment test"
                        expected = True
                    actual `shouldBe` expected

                it "# comment # in comment" $ do
                    let actual = _isComment "# comment # in comment"
                        expected = True
                    actual `shouldBe` expected

            describe "_isSpace" $ do
                it "___" $ do
                    let actual = _isSpace "   "
                        expected = True
                    actual `shouldBe` expected

            describe "_isNewline" $ do
                it "newline newline" $ do
                    let actual = _isNewline "\n"
                        expected = True
                    actual `shouldBe` expected


