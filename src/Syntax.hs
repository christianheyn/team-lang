{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Syntax (
      isParamterList
    , isList
    , AST_NODE_TYPE(..)
    , AST_NODE(..)
    ) where

    import Tokenizer (
          generateTokens
        , TokenType(..)
        , Token(..)
        )
    import GHC.Generics
    import Data.Data (Typeable)
    import Data.List (find, null, or)
    import Data.Maybe (isJust, fromJust)
    import Control.Lens

    data AST_NODE_TYPE =
          AstPrimitiv
        | AstSymbol
        | AstParameter
        | AstParameterList
        | AstLambda
        | AstFunction
        | AstFunctionCall
        | AstFunCall
        | AstList
        | AstOpen
        | AstClose
        | AstError
        deriving (Show, Eq)

    data AST_NODE = AST_NODE {
          _astNodeType :: AST_NODE_TYPE
        , _astTokens   :: [Token]
        , _astChildren :: [AST_NODE]
        } deriving (Show, Eq)

    type Check = ([Token] -> ([AST_NODE], [Token]))

    createAstNode astType tokens childrens = AST_NODE {
          _astNodeType = astType
        , _astTokens   = tokens
        , _astChildren = childrens
        }

    -- QUANTIFIER =============================================================

    qZeroOrMore' :: [Check] -> [Token] -> ([AST_NODE], [Token])
    qZeroOrMore' _      []     = ([], [])
    qZeroOrMore' checks tokens = if isJust match
                                  then (astNodes ++ nextAstNodes, finalTokens)
                                  else ([], tokens)
        where results = map (\c -> c tokens) checks -- mapUntil
              notEmpty (a, _) = ((not . null) a) && ((not . hasAstError) a)
              match    = find notEmpty results
              (astNodes, nextTokens) = fromJust match
              (nextAstNodes, finalTokens) = qZeroOrMore' checks nextTokens

    qZeroOrMore :: [Check] -> Check
    qZeroOrMore checks = qZeroOrMore' checks

    qOneOrMore' :: [Check] -> [Token] -> ([AST_NODE], [Token])
    qOneOrMore' _      []     = ([], [])
    qOneOrMore' checks tokens = if isJust match
                                  then (astNodes ++ nextAstNodes, finalTokens)
                                  else ([createAstNode AstError [] []], [])
        where results = map (\c -> c tokens) checks
              notEmpty (a, _) = ((not . null) a) && ((not . hasAstError) a)
              match    = find notEmpty results -- TODO: Sort most ast nodes
              (astNodes, nextTokens) = fromJust match
              (nextAstNodes, finalTokens) = qZeroOrMore' checks nextTokens

    qOneOrMore :: [Check] -> Check
    qOneOrMore checks = qOneOrMore' checks

    -- TODO: qZeroOrOne :: [Check] -> Check

    qExact' :: [Check] -> [Token] -> ([AST_NODE], [Token])
    qExact' []     ts     = ([], ts)
    qExact' _      []     = ([], [])
    qExact' (c:cs) tokens = if (hasAstError ast)
                             then (ast, restTokens)
                             else (ast ++ nextAst, nextRestTokens)
        where (ast, restTokens) = c tokens
              (nextAst, nextRestTokens) = qExact' cs restTokens

    qExact :: [Check] -> Check
    qExact checks = qExact' checks

    -- END QUANTIFIER ========================================================

    checkEnd ts = ([createAstNode AstError ts []] , [])

    _isPrimitive :: Check
    _isPrimitive []           = checkEnd []
    _isPrimitive allTs@(t:ts) = if (_TType t `elem` [
                                                      T_String
                                                    , T_Number
                                                    , T_ComplexNumber
                                                    , T_BooleanTrue
                                                    , T_BooleanFalse
                                                    ]
                                )
                                then ([createAstNode AstPrimitiv [t] []], ts)
                                else checkEnd [t]

    _isSymbol :: Check
    _isSymbol []           = checkEnd []
    _isSymbol allTs@(t:ts) = if (_TType t == T_Symbol)
                  then ([createAstNode AstSymbol [t] []], ts)
                  else checkEnd [t]

    _isParameter :: Check
    _isParameter []           = checkEnd []
    _isParameter allTs@(t:ts) = if (_TType t `elem` [T_Symbol, T_NamedParameter])
                  then ([createAstNode AstParameter [t] []], ts)
                  else checkEnd [t]

    _isOpenRound :: Check
    _isOpenRound []           = checkEnd []
    _isOpenRound allTs@(t:ts) = if (_TType t == T_OpenRoundBracket)
                  then ([createAstNode AstOpen [t] []], ts)
                  else checkEnd [t]

    _isClosingRound :: Check
    _isClosingRound []           = checkEnd []
    _isClosingRound allTs@(t:ts) = if (_TType t == T_ClosingRoundBracket)
                  then ([createAstNode AstClose [t] []], ts)
                  else checkEnd [t]

    _isOpenSquare :: Check
    _isOpenSquare []           = checkEnd []
    _isOpenSquare allTs@(t:ts) = if (_TType t == T_OpenSquareBracket)
                  then ([createAstNode AstOpen [t] []], ts)
                  else checkEnd [t]

    _isClosingSquare :: Check
    _isClosingSquare []           = checkEnd []
    _isClosingSquare allTs@(t:ts) = if (_TType t == T_ClosingSquareBracket)
                  then ([createAstNode AstClose [t] []], ts)
                  else checkEnd [t]

    hasAstError [] = False
    hasAstError as = or (map go as)
        where go a = if _astNodeType a == AstError
                     then True
                     else hasAstError (_astChildren a)

    withRoundGroup :: [Check] -> [Token] -> ([AST_NODE], [Token])
    withRoundGroup checks tokens = if hasError
                                   then (nodes, [])
                                   else ([astResult], restTokens)
        where (nodes, restTokens) = qExact ([_isOpenRound] ++ checks ++ [_isClosingRound]) tokens
              innerNodes = (init . tail) nodes
              hasError = hasAstError nodes
              astResult = createAstNode AstParameterList [] innerNodes

    withSquareGroup :: [Check] -> [Token] -> ([AST_NODE], [Token])
    withSquareGroup checks tokens = if hasError
                                   then (nodes, [])
                                   else ([astResult], restTokens)
        where (nodes, restTokens) = qExact ([ _isOpenSquare] ++ checks ++ [_isClosingSquare]) tokens
              innerNodes = (init . tail) nodes
              hasError = hasAstError nodes
              astResult = createAstNode AstList [] innerNodes

    isParamterList :: Check
    isParamterList = withRoundGroup [qZeroOrMore [ _isParameter, isParamterList ]]

    isList :: Check
    isList = withSquareGroup [qZeroOrMore [isList, _isSymbol, _isPrimitive]]

