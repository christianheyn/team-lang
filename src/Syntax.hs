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
    import Data.List (find, null)
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

    checkEnd ts = ([] , [])

    _isPrimitive :: Check
    _isPrimitive []     = checkEnd []
    _isPrimitive allTs@(t:ts) = if (_TType t `elem` [T_String, T_Number, T_BooleanTrue, T_BooleanFalse, T_Lens])
                  then ([AstPrimitiv [t] []], ts)
                  else ([] , allTs)

    _isParameter :: Check
    _isParameter []     = checkEnd []
    _isParameter allTs@(t:ts) = if (_TType t `elem` [T_Symbol, T_NamedParameter])
                  then ([AstParameter [t] []], ts)
                  else ([] , allTs)

    _isOpenRound :: Check
    _isOpenRound []     = checkEnd []
    _isOpenRound allTs@(t:ts) = if (_TType t == T_OpenRoundBracket)
                  then ([AstIgnore [t] []], ts)
                  else ([] , allTs)

    _isClosingRound :: Check
    _isClosingRound []     = checkEnd []
    _isClosingRound allTs@(t:ts) = if (_TType t == T_ClosingRoundBracket)
                  then ([AstIgnore [t] []], ts)
                  else ([] , allTs)

    matches [] = False
    matches _  = True

    isZeroOrMore' :: [Check] -> [Token] -> ([AST_NODE], [Token])
    isZeroOrMore' _      []     = ([], [])
    isZeroOrMore' checks tokens = if isJust match
                                  then (astNodes ++ nextAstNodes, finalTokens)
                                  else ([], tokens)
        where results = map (\c -> c tokens) checks -- mapUntil
              notEmpty (a, _) = (not . null) a
              match    = find notEmpty results
              (astNodes, nextTokens) = fromJust match
              (nextAstNodes, finalTokens) = isZeroOrMore' checks nextTokens

    isZeroOrMore :: [Check] -> Check
    isZeroOrMore checks = isZeroOrMore' checks

    isParamterList :: Check
    isParamterList ts =
        isZeroOrMore [_isOpenRound, _isParameter, _isClosingRound] ts

    -- isExact     :: [Check] -> Check
    -- isOr        :: [Check] -> Check
    -- isZeroOrOne :: [Check] -> Check
    -- isOneOrMore :: [Check] -> Check
    -- isZeroOrMore :: [Check] -> Check
