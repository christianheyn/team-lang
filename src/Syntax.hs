{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Syntax (
    ) where

    import Tokenizer (
          generateTokens
        , TokenType(..)
        , Token(..)
        )
    import GHC.Generics
    import Data.Data (Typeable)
    import Util.Regex
    import qualified Data.ByteString.Lazy.Char8 as L
    import Control.Lens

    data SyntaxType =
          SyntaxAtom
        | SyntaxExpression
        | SyntaxFunction
        | SyntaxLambda
        | SyntaxIfThenElse
        | SyntaxImport
        | SyntaxExport
        | SyntaxBody
        | SyntaxList
        | SyntaxTuple
        deriving (Show, Eq, Generic, Typeable)

    data SyntaxNode = SyntaxNode {
          _SNType     :: SyntaxType
        , _SNTokens   :: [Token]
        , _SNIndex    :: Int
        , _SNChildren :: [SyntaxNode]
        , _SNScope    :: L.ByteString
        } deriving (Show, Eq, Generic, Typeable)

    makeLenses ''SyntaxNode
