{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module AST (
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

    data ASTType =
          AST_File
        | AST_Atom
        | AST_FunctionDeclaration
        | AST_FunctionCall
        | AST_ListDeclaration
        | AST_Import
        | AST_ImportAs
        | AST_Export
        deriving (Show, Eq, Generic, Typeable)

    data AST = AST {
          _ASTType      :: ASTType
        , _ASTFlags     :: [Token]
        , _ASTChildren  :: [AST]
        , _ASTDeepLevel :: Int
        } deriving (Show, Eq, Generic, Typeable)

    makeLenses ''AST

    type ASTIndex = Int
    type ParentAstType = ASTType
    type CTX = (ParentAstType, ASTIndex)

    toAst :: [Token] -> CTX -> [AST]
    toAst []     _    = []
    toAst (x:xs) ctx  = []
        where astType = fst ctx
              i       = snd ctx
