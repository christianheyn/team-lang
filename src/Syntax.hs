{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Syntax (
      isParamterList
    , isList
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

    data AST_NODE = -- TODO Record syntax
          AstPrimitiv      [Token] [AST_NODE]
        | AstSymbol        [Token] [AST_NODE]
        | AstParameter     [Token] [AST_NODE]
        | AstParameterList [Token] [AST_NODE]
        | AstLambda        [Token] [AST_NODE]
        | AstFunction      [Token] [AST_NODE]
        | AstFunctionCall  [Token] [AST_NODE]
        | AstFunCall       [Token] [AST_NODE]
        | AstList          [Token] [AST_NODE]

        | AstOpen          [Token] [AST_NODE]
        | AstClose         [Token] [AST_NODE]
        | AstError         [Token] [AST_NODE]
        deriving (Show, Eq)

    type Check = ([Token] -> ([AST_NODE], [Token]))

    childNodes (AstPrimitiv      _ ns) = ns
    childNodes (AstSymbol        _ ns) = ns
    childNodes (AstParameter     _ ns) = ns
    childNodes (AstParameterList _ ns) = ns
    childNodes (AstLambda        _ ns) = ns
    childNodes (AstFunction      _ ns) = ns
    childNodes (AstFunctionCall  _ ns) = ns
    childNodes (AstFunCall       _ ns) = ns
    childNodes (AstList          _ ns) = ns
    childNodes (AstOpen          _ ns) = ns
    childNodes (AstClose         _ ns) = ns
    childNodes (AstError         _ ns) = ns


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
                                  else ([AstError [] []], [])
        where results = map (\c -> c tokens) checks -- mapUntil
              notEmpty (a, _) = ((not . null) a) && ((not . hasAstError) a)
              match    = find notEmpty results
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

    checkEnd ts = ([AstError ts []] , [])


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
                                then ([AstPrimitiv [t] []], ts)
                                else checkEnd [t]

    _isSymbol :: Check
    _isSymbol []           = checkEnd []
    _isSymbol allTs@(t:ts) = if (_TType t == T_Symbol)
                  then ([AstSymbol [t] []], ts)
                  else checkEnd [t]

    _isParameter :: Check
    _isParameter []           = checkEnd []
    _isParameter allTs@(t:ts) = if (_TType t `elem` [T_Symbol, T_NamedParameter])
                  then ([AstParameter [t] []], ts)
                  else checkEnd [t]

    _isOpenRound :: Check
    _isOpenRound []           = checkEnd []
    _isOpenRound allTs@(t:ts) = if (_TType t == T_OpenRoundBracket)
                  then ([AstOpen [t] []], ts)
                  else checkEnd [t]

    _isClosingRound :: Check
    _isClosingRound []           = checkEnd []
    _isClosingRound allTs@(t:ts) = if (_TType t == T_ClosingRoundBracket)
                  then ([AstClose [t] []], ts)
                  else checkEnd [t]

    _isOpenSquare :: Check
    _isOpenSquare []           = checkEnd []
    _isOpenSquare allTs@(t:ts) = if (_TType t == T_OpenSquareBracket)
                  then ([AstOpen [t] []], ts)
                  else checkEnd [t]

    _isClosingSquare :: Check
    _isClosingSquare []           = checkEnd []
    _isClosingSquare allTs@(t:ts) = if (_TType t == T_ClosingSquareBracket)
                  then ([AstClose [t] []], ts)
                  else checkEnd [t]

    hasAstError [] = False
    hasAstError as = or (map go as)
        where go (AstError _ _) = True
              go ns = hasAstError (childNodes ns)

    withRoundGroup :: [Check] -> [Token] -> ([AST_NODE], [Token])
    withRoundGroup checks tokens = if hasError
                                   then (nodes, [])
                                   else ([astResult], restTokens)
        where (nodes, restTokens) = qExact ([_isOpenRound] ++ checks ++ [_isClosingRound]) tokens
              innerNodes = (init . tail) nodes
              hasError = hasAstError nodes
              astResult = AstParameterList [] innerNodes

    withSquareGroup :: [Check] -> [Token] -> ([AST_NODE], [Token])
    withSquareGroup checks tokens = if hasError
                                   then (nodes, [])
                                   else ([astResult], restTokens)
        where (nodes, restTokens) = qExact ([ _isOpenSquare] ++ checks ++ [_isClosingSquare]) tokens
              innerNodes = (init . tail) nodes
              hasError = hasAstError nodes
              astResult = AstList [] innerNodes

    isParamterList :: Check
    isParamterList = withRoundGroup [qZeroOrMore [ _isParameter, isParamterList ]]

    isList :: Check
    isList = withSquareGroup [qZeroOrMore [isList, _isSymbol, _isPrimitive]]
