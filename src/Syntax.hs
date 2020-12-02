{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Syntax (
      isParamterList
    , isFunction
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

    qOr' :: [Check] -> [Token] -> ([AST_NODE], [Token])
    qOr' []     ts     = ([createAstNode AstError [] []], ts)
    qOr' _      []     = ([], [])
    qOr' (c:cs) tokens = if (hasAstError ast)
                         then qOr' cs tokens
                         else (ast, restTokens)
        where (ast, restTokens) = c tokens

    qOr :: [Check] -> Check
    qOr checks = qOr' checks

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
                                                    -- , T_VOID
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

    _isOpenCurly :: Check
    _isOpenCurly []           = checkEnd []
    _isOpenCurly allTs@(t:ts) = if (_TType t == T_OpenCurlyBracket)
                  then ([createAstNode AstOpen [t] []], ts)
                  else checkEnd [t]

    _isClosingCurly :: Check
    _isClosingCurly []           = checkEnd []
    _isClosingCurly allTs@(t:ts) = if (_TType t == T_ClosingCurlyBracket)
                  then ([createAstNode AstClose [t] []], ts)
                  else checkEnd [t]

    hasAstError [] = False
    hasAstError as = or (map go as)
        where go a = if _astNodeType a == AstError
                     then True
                     else hasAstError (_astChildren a)

    withRoundGroup :: AST_NODE_TYPE -> [Check] -> [Token] -> ([AST_NODE], [Token])
    withRoundGroup t checks tokens = if hasError
                                   then (nodes, [])
                                   else ([astResult], restTokens)
        where (nodes, restTokens) = qExact ([_isOpenRound] ++ checks ++ [_isClosingRound]) tokens
              innerNodes = (init . tail) nodes
              hasError = hasAstError nodes
              astResult = createAstNode t [] innerNodes

    withSquareGroup :: [Check] -> [Token] -> ([AST_NODE], [Token])
    withSquareGroup checks tokens = if hasError
                                   then (nodes, [])
                                   else ([astResult], restTokens)
        where (nodes, restTokens) = qExact ([ _isOpenSquare] ++ checks ++ [_isClosingSquare]) tokens
              innerNodes = (init . tail) nodes
              hasError = hasAstError nodes
              astResult = createAstNode AstList [] innerNodes

    isParamterList :: Check
    isParamterList = withRoundGroup AstParameterList [qZeroOrMore [ _isParameter, isParamterList ]]

    isFunctionCall :: Check
    isFunctionCall = withRoundGroup AstFunctionCall [_isSymbol, qZeroOrMore [ _isSymbol, _isPrimitive, isFunctionCall ]]

    isList :: Check
    isList = withSquareGroup [qZeroOrMore [isList, _isSymbol, _isPrimitive]]

    isFunction :: [Token] -> ([AST_NODE], [Token])
    isFunction tokens = if hasError
                                   then (nodes, [])
                                   else ([astResult], restTokens)
        where (nodes, restTokens) = qExact ([
                                              _isOpenCurly
                                            , _isSymbol
                                            , isParamterList
                                            , qOr [
                                                    _isPrimitive
                                                  , _isSymbol
                                                  , isFunctionCall
                                                  -- , isLambda
                                                  ]
                                            , _isClosingCurly]) tokens
              innerNodes = (init . tail) nodes
              hasError = hasAstError nodes
              astResult = createAstNode AstFunction [] innerNodes
