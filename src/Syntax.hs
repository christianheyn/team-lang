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
          SyntaxFile
        | SyntaxAtom
        | SyntaxExpression
        | SyntaxFunction
        | SyntaxImport
        | SyntaxExport
        | SyntaxList
        deriving (Show, Eq, Generic, Typeable)


    data SyntaxNode = SyntaxNode {
          _syntaxNodeId       :: L.ByteString
        , _syntaxNodeType     :: SyntaxType
        , _syntaxNodeTokens   :: [Token]
        , _syntaxNodeChildren :: [SyntaxNode]
        } deriving (Show, Eq, Generic, Typeable)

    makeLenses ''SyntaxNode
