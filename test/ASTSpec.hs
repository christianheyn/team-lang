{-# LANGUAGE OverloadedStrings #-}

module ASTSpec where

    import Test.Hspec
    import Text.RawString.QQ
    import qualified Data.ByteString.Lazy.Char8 as L
    import Tokenizer
    import Syntax

    spec :: Spec
    spec = do
        describe "Syntax" $ do
            it "try" $ do
                let actual = isParamterList $ generateTokens "(a, b)"
                    expected = ([], [])
                actual `shouldBe` expected
