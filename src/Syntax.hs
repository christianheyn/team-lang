{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Syntax (
      isParamterList
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

    data AST_NODE =
          AstPrimitiv      [Token] [AST_NODE]
        | AstParameter     [Token] [AST_NODE]
        | AstParameterList [Token] [AST_NODE]
        | AstLambda        [Token] [AST_NODE]
        | AstFunction      [Token] [AST_NODE]
        | AstFunctionCall  [Token] [AST_NODE]
        | AstFunCall       [Token] [AST_NODE]
        | AstList          [Token] [AST_NODE]

        | AstIgnore        [Token] [AST_NODE]
        | AstError         [Token] [AST_NODE]
        deriving (Show, Eq)

    type Check = ([Token] -> ([AST_NODE], [Token]))

    checkEnd ts = ([AstError ts []] , [])

    _isPrimitive :: Check
    _isPrimitive []           = checkEnd []
    _isPrimitive allTs@(t:ts) = if (_TType t `elem` [T_String, T_Number, T_BooleanTrue, T_BooleanFalse, T_Lens])
                  then ([AstPrimitiv [t] []], ts)
                  else checkEnd [t]

    _isParameter :: Check
    _isParameter []           = checkEnd []
    _isParameter allTs@(t:ts) = if (_TType t `elem` [T_Symbol, T_NamedParameter])
                  then ([AstParameter [t] []], ts)
                  else checkEnd [t]

    _isOpenRound :: Check
    _isOpenRound []           = checkEnd []
    _isOpenRound allTs@(t:ts) = if (_TType t == T_OpenRoundBracket)
                  then ([AstIgnore [t] []], ts)
                  else checkEnd [t]

    _isClosingRound :: Check
    _isClosingRound []           = checkEnd []
    _isClosingRound allTs@(t:ts) = if (_TType t == T_ClosingRoundBracket)
                  then ([AstIgnore [t] []], ts)
                  else checkEnd [t]

    -- roundGroup [Check] -> Check

    -- QUANTIFIER =============================================================
    hasAstError ast = or (map go ast)
        where go (AstError _ _) = True
              go _              = False

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

    isParamterList :: Check
    isParamterList =
        qExact [
             _isOpenRound
            , qZeroOrMore [ _isParameter, _isPrimitive, isParamterList ]
            , _isClosingRound
            ]

    -- isOr        :: [Check] -> Check
    -- isOneOrMore :: [Check] -> Check
