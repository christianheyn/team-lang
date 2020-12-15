{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Syntax (
      isParamterList
    , isFunction
    , isLambda
    , isList
    , isEnum
    , isEnumValue
    , isTemplateType
    , isPropListType
    , isJsonType
    , isClass
    , isTypeDefinition
    , isIfThenElse
    , isImport
    , isLet
    , isVar
    , isTypeAlias
    , hasAstError
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
          AstPrimitiv           -- 3 , "string", true
        | AstSymbol             -- a
        | AstTypeAlias          -- (type J { "a" Number })
        | AstTypeDefinition     -- <T> <U> T -> {T -> [U]}
        | AstTypeSymbol         -- T
        | AstImportedTypeSymbol -- tdd.Test
        | AstRestType           -- @Test
        | AstTemplateType       -- <T>
        | AstMaybeType          -- maybe T
        | AstFunctionType       -- {T -> U}
        | AstListType           -- [T]
        | AstJsonType           -- { "key" Value "key2" { "key3" String } "key4" [Number] }
        | AstJsonKeyValueType   -- "key" Value
        | AstJsonArrayType      -- [T]
        | AstClassFunction      -- fmap <U> {T -> U} -> [T] -> [U]
        | AstClass              -- class Functor <T> { fmap <U> {T -> U} -> [T] -> [U] }
        | AstProp               -- a:
        | AstPropKeyValueType   -- a: Number
        | AstPropListType       -- [a: Number]
        | AstParameter          -- a
        | AstParameterList      -- (a b)
        | AstEnum               -- (enum Hallo :hi :hello :huhu)
        | AstEnumMember         -- :hi
        | AstEnumValue          -- Hallo:hi, imported.Hallo:hi
        | AstLambda             -- {(a) (+ a 1)}
        | AstFunction           -- {plus1 (a) (+ a 1)}
        | AstFunctionBody
        | AstFunctionCall       -- (plus1 3)
        | AstList               -- [1 2 3 (+ 2 5)]
        | AstIfCondition        -- (if a then b else c)
        | AstVar                -- (var CNumber a 6+4i)
        | AstLet                -- (let (type T [Number]) (var Number a 3) (print a))
        | AstOpen               -- ({[
        | AstClose              -- ]})
        | AstImport
        | AstImportAs
        | AstError
        deriving (Show, Eq)

    data AST_NODE = AST_NODE {
          _astNodeType :: AST_NODE_TYPE
        , _astTokens   :: [Token]
        , _astChildren :: [AST_NODE]
        } deriving (Show, Eq)

    type AstFn = ([Token] -> ([AST_NODE], [Token]))

    createAstNode astType tokens childrens = AST_NODE {
          _astNodeType = astType
        , _astTokens   = tokens
        , _astChildren = childrens
        }

    hasAstError [] = False
    hasAstError as = or (map go as)
        where go a = if _astNodeType a == AstError
                     then True
                     else hasAstError (_astChildren a)

    -- QUANTIFIER =============================================================

    qZeroOrMore' :: [AstFn] -> [Token] -> ([AST_NODE], [Token])
    qZeroOrMore' _      []     = ([], [])
    qZeroOrMore' checks tokens = if isJust match
                                  then (astNodes ++ nextAstNodes, finalTokens)
                                  else ([], tokens)
        where results = map (\c -> c tokens) checks -- mapUntil
              notEmpty (a, _) = ((not . null) a) && ((not . hasAstError) a)
              match    = find notEmpty results
              (astNodes, nextTokens) = fromJust match
              (nextAstNodes, finalTokens) = qZeroOrMore' checks nextTokens

    qZeroOrMore :: [AstFn] -> AstFn
    qZeroOrMore checks = qZeroOrMore' checks

    qOneOrMore' :: [AstFn] -> AstFn
    qOneOrMore' _      []     = ([], [])
    qOneOrMore' checks tokens =
        if isJust match
        then (astNodes ++ nextAstNodes, finalTokens)
        else ([createAstNode AstError [] []], [])
        where results = map (\c -> c tokens) checks
              notEmpty (a, _) = ((not . null) a) && ((not . hasAstError) a)
              match    = find notEmpty results
              (astNodes, nextTokens) = fromJust match
              (nextAstNodes, finalTokens) = qZeroOrMore' checks nextTokens

    qOneOrMore :: [AstFn] -> AstFn
    qOneOrMore checks = qOneOrMore' checks

    qOptional :: AstFn -> AstFn
    qOptional check tokens =
        if (not . hasAstError) nodes
        then (nodes, restTokens)
        else ([], tokens)
        where (nodes, restTokens) = check tokens

    qExact' :: [AstFn] -> [Token] -> ([AST_NODE], [Token])
    qExact' []     ts     = ([], ts)
    qExact' _      []     = ([], [])
    qExact' (c:cs) tokens = if (hasAstError ast)
                            then (ast, restTokens)
                            else (ast ++ nextAst, nextRestTokens)
        where (ast, restTokens) = c tokens
              (nextAst, nextRestTokens) = qExact' cs restTokens

    qExact :: [AstFn] -> AstFn
    qExact checks = qExact' checks

    qOr' :: [AstFn] -> [Token] -> ([AST_NODE], [Token])
    qOr' []     ts     = ([createAstNode AstError [] []], ts)
    qOr' _      []     = ([], [])
    qOr' (c:cs) tokens = if (hasAstError ast)
                         then qOr' cs tokens
                         else (ast, restTokens)
        where (ast, restTokens) = c tokens

    qOr :: [AstFn] -> AstFn
    qOr checks = qOr' checks

    -- END QUANTIFIER ========================================================

    checkEnd ts = ([createAstNode AstError ts []] , []) -- TODO: AstError msg

    _isString :: AstFn
    _isString []     = checkEnd []
    _isString (t:ts) =
        if (_TType t == T_String)
        then ([createAstNode AstPrimitiv [t] []], ts)
        else checkEnd [t]

    _isBool :: AstFn
    _isBool []     = checkEnd []
    _isBool (t:ts) =
        if (_TType t `elem` [T_BooleanTrue, T_BooleanFalse])
        then ([createAstNode AstPrimitiv [t] []], ts)
        else checkEnd [t]

    _isPrimitive :: AstFn
    _isPrimitive []     = checkEnd []
    _isPrimitive (t:ts) =
        if (_TType t `elem` [
              T_String
            , T_Number
            , T_ComplexNumber
            , T_BooleanTrue
            , T_BooleanFalse
            , T_Void
            ])
        then ([createAstNode AstPrimitiv [t] []], ts)
        else checkEnd [t]

    _isSymbol :: AstFn
    _isSymbol []     = checkEnd []
    _isSymbol (t:ts) =
        if (_TType t == T_Symbol)
        then ([createAstNode AstSymbol [t] []], ts)
        else checkEnd [t]

    -- TODO: isImportedSymbol :: AstFn -- tdd.describe

    _isType' :: AstFn
    _isType' []     = checkEnd []
    _isType' (t:ts) =
        if (_TType t == T_Type)
        then ([createAstNode AstTypeSymbol [t] []], ts)
        else checkEnd [t]

    _isImportedType' :: AstFn
    _isImportedType' []            = checkEnd []
    _isImportedType' [_, _]        = checkEnd []
    _isImportedType' (t:t':t'':ts) =
        if (_TType t == T_Symbol && _TType t' == T_ReferenceDot && _TType t'' == T_Type)
        then ([createAstNode AstImportedTypeSymbol (t:t':t'':[]) []], ts)
        else checkEnd (t:t':t'':[])

    _isRestSpread :: AstFn -- @
    _isRestSpread []     = checkEnd []
    _isRestSpread (t:ts) =
        if (_TType t == T_RestSpread)
        then ([], ts)
        else checkEnd [t]

    _isRestType :: AstFn -- @T , @imported.T, @{T -> T}
    _isRestType tokens =
        if hasError
        then (nodes, [])
        else ([astResult], restTokens)
        where (nodes, restTokens) = qExact [_isRestSpread, qOr [_isType, isFunctionTypeDef, isListType, isPropListType, isMaybeType]] tokens
              hasError = hasAstError nodes
              astResult = createAstNode AstRestType [] nodes

    _isType :: AstFn
    _isType = qOr [_isType', _isImportedType']

    _isProp :: AstFn
    _isProp []     = checkEnd []
    _isProp (t:ts) =
        if (_TType t `elem` [T_Prop])
        then ([createAstNode AstProp [t] []], ts)
        else checkEnd [t]

    _hasTokenType :: TokenType -> AstFn
    _hasTokenType ttype []     = checkEnd []
    _hasTokenType ttype (t:ts) =
        if (_TType t == ttype)
        then ([], ts)
        else checkEnd [t]

    _isParameter :: AstFn
    _isParameter []     = checkEnd []
    _isParameter (t:ts) =
        if (_TType t == T_Symbol)
        then ([createAstNode AstParameter [t] []], ts)
        else checkEnd [t]

    _isOpenRound :: AstFn
    _isOpenRound []     = checkEnd []
    _isOpenRound (t:ts) =
        if (_TType t == T_OpenRoundBracket)
        then ([createAstNode AstOpen [t] []], ts)
        else checkEnd [t]

    _isClosingRound :: AstFn
    _isClosingRound []     = checkEnd []
    _isClosingRound (t:ts) =
        if (_TType t == T_ClosingRoundBracket)
        then ([createAstNode AstClose [t] []], ts)
        else checkEnd [t]

    _isOpenSquare :: AstFn
    _isOpenSquare []     = checkEnd []
    _isOpenSquare (t:ts) =
        if (_TType t == T_OpenSquareBracket)
        then ([createAstNode AstOpen [t] []], ts)
        else checkEnd [t]

    _isClosingSquare :: AstFn
    _isClosingSquare []     = checkEnd []
    _isClosingSquare (t:ts) =
        if (_TType t == T_ClosingSquareBracket)
        then ([createAstNode AstClose [t] []], ts)
        else checkEnd [t]

    _isOpenCurly :: AstFn
    _isOpenCurly []     = checkEnd []
    _isOpenCurly (t:ts) =
        if (_TType t == T_OpenCurlyBracket)
        then ([createAstNode AstOpen [t] []], ts)
        else checkEnd [t]

    _isClosingCurly :: AstFn
    _isClosingCurly []     = checkEnd []
    _isClosingCurly (t:ts) =
        if (_TType t == T_ClosingCurlyBracket)
        then ([createAstNode AstClose [t] []], ts)
        else checkEnd [t]

    withRoundGroup :: AST_NODE_TYPE -> [AstFn] -> AstFn
    withRoundGroup t checks tokens =
        if hasError
        then (nodes, [])
        else ([astResult], restTokens)
        where (nodes, restTokens) = qExact ([_isOpenRound] ++ checks ++ [_isClosingRound]) tokens
              innerNodes = (init . tail) nodes
              hasError = hasAstError nodes
              astResult = createAstNode t [] innerNodes

    withCurlyGroup :: AST_NODE_TYPE -> [AstFn] -> AstFn
    withCurlyGroup t checks tokens =
        if hasError
        then (nodes, [])
        else ([astResult], restTokens)
        where (nodes, restTokens) = qExact ([_isOpenCurly] ++ checks ++ [_isClosingCurly]) tokens
              innerNodes = (init . tail) nodes
              hasError = hasAstError nodes
              astResult = createAstNode t [] innerNodes

    withSquareGroup :: AST_NODE_TYPE -> [AstFn] -> AstFn
    withSquareGroup astType checks tokens =
        if hasError
        then (nodes, [])
        else ([astResult], restTokens)
        where (nodes, restTokens) = qExact ([ _isOpenSquare] ++ checks ++ [_isClosingSquare]) tokens
              innerNodes = (init . tail) nodes
              hasError = hasAstError nodes
              astResult = createAstNode astType [] innerNodes

    isParamterList :: AstFn
    isParamterList = withRoundGroup AstParameterList [qZeroOrMore [ _isParameter, isParamterList ]]

    isFunctionCall :: AstFn
    isFunctionCall = withRoundGroup AstFunctionCall [_isSymbol, qZeroOrMore [ _isSymbol, _isPrimitive, isFunctionCall, isLambda ]]

    isList :: AstFn
    isList = withSquareGroup AstList [qZeroOrMore [isList, _isSymbol, _isPrimitive]]

    _isEnumMember :: AstFn
    _isEnumMember []     = checkEnd []
    _isEnumMember (t:ts) =
        if (_TType t == T_EnumMember)
        then ([createAstNode AstEnumMember [t] []], ts)
        else checkEnd [t]

    isEnum :: AstFn
    isEnum = withRoundGroup AstEnum [
          _hasTokenType T_EnumKeyword
        , _isType
        , qExact [_isEnumMember]
        , qZeroOrMore [ _isEnumMember ]
        ]

    isEnumValue :: AstFn
    isEnumValue tokens =
        if hasAstError nodes
        then (nodes, [])
        else ([astResult], restTokens)
        where (nodes, restTokens) = check tokens
              astResult = createAstNode AstEnumValue [] nodes
              check = qExact [
                            _isType
                          , _isEnumMember
                          ]

    isFunctionBody :: AstFn
    isFunctionBody tokens =
        if hasAstError nodes
        then (nodes, [])
        else ([astResult], restTokens)
        where (nodes, restTokens) = check tokens
              astResult = createAstNode AstFunctionBody [] nodes
              check = qOr [
                            _isPrimitive
                          , _isSymbol
                          , isFunctionCall
                          , isLambda
                          ]

    isLambda :: AstFn
    isLambda tokens =
        if hasError
        then (nodes, [])
        else ([astResult], restTokens)
        where (nodes, restTokens) = qExact ([
                                              _isOpenCurly
                                            , qOptional isTypeDefinition
                                            , isParamterList
                                            , isFunctionBody
                                            , _isClosingCurly]) tokens
              innerNodes = (init . tail) nodes
              hasError = hasAstError nodes
              astResult = createAstNode AstLambda [] innerNodes

    isFunction :: AstFn
    isFunction tokens =
        if hasError
        then (nodes, [])
        else ([astResult], restTokens)
        where (nodes, restTokens) = qExact ([
                                              _isOpenCurly
                                            , _isSymbol
                                            , qOptional isTypeDefinition
                                            , isParamterList
                                            , isFunctionBody
                                            -- TODO: , qOptional isCatchFunction
                                            , _isClosingCurly]) tokens
              innerNodes = (init . tail) nodes
              hasError = hasAstError nodes
              astResult = createAstNode AstFunction [] innerNodes

    _isOpenXML :: AstFn
    _isOpenXML []     = checkEnd []
    _isOpenXML (t:ts) =
        if (_TType t == T_Symbol && _TValue t == "<" )
        then ([], ts)
        else checkEnd [t]

    _isClosingXML :: AstFn
    _isClosingXML []     = checkEnd []
    _isClosingXML (t:ts) =
        if (_TType t == T_Symbol && _TValue t == ">" )
        then ([], ts)
        else checkEnd [t]

    isTemplateType :: AstFn -- <T>
    isTemplateType tokens =
        if hasError
        then (nodes, [])
        else ([astResult], restTokens)
        where (nodes, restTokens) = qExact [_isOpenXML, _isType, _isClosingXML] tokens
              hasError = hasAstError nodes
              astResult = createAstNode AstTemplateType [] nodes

    isMaybeType :: AstFn
    isMaybeType tokens =
        if hasError
        then (nodes, [])
        else ([astResult], restTokens)
        where (nodes, restTokens) = qExact [_hasTokenType T_MaybeType, allTypes] tokens
              hasError = hasAstError nodes
              astResult = createAstNode AstMaybeType [] nodes
              allTypes = qOr [_isType, isFunctionTypeDef, isListType, isPropListType, isMaybeType]

    isListType :: AstFn
    isListType tokens =
        if hasError
        then (nodes, [])
        else ([astResult], restTokens)
        where (nodes, restTokens) = qExact [
                      _isOpenSquare
                    , qOr [_isType, isFunctionTypeDef, isListType]
                    , _isClosingSquare]
                     tokens
              innerNodes = (init . tail) nodes
              hasError = hasAstError nodes
              astResult = createAstNode AstListType [] innerNodes

    isArrowTypes :: AstFn -- T -> @U -> U, @T -> T
    isArrowTypes = qOr [
            qExact [
                _isRestType
                , _hasTokenType T_ArrowLeft
                , allTypes
            ],
            qExact [
                qZeroOrMore [
                    qExact [
                        allTypes
                        , _hasTokenType T_ArrowLeft
                        ]
                ]
                , qOr [
                    qExact [
                        _isRestType
                        , _hasTokenType T_ArrowLeft
                        , allTypes
                    ]
                    , allTypes
                ]
            ]
        ]

        where allTypes = qOr [
                              _isType
                            , isFunctionTypeDef
                            , isListType
                            , isPropListType
                            , isMaybeType
                            , isJsonType]

    isFunctionTypeDef :: AstFn -- {T -> {T -> U} -> U}
    isFunctionTypeDef = withCurlyGroup AstFunctionType [isArrowTypes]

    isTypeDefinition :: AstFn
    isTypeDefinition tokens =
        if hasError
        then (nodes, [])
        else ([astResult], restTokens)
        where (nodes, restTokens) = qExact [
                                          qZeroOrMore [isTemplateType]
                                        , isArrowTypes
                                        ] tokens
              hasError = hasAstError nodes
              astResult = createAstNode AstTypeDefinition [] nodes

    isTypeDefinitionWithoutTemplates :: AstFn
    isTypeDefinitionWithoutTemplates tokens =
        if hasError
        then (nodes, [])
        else ([astResult], restTokens)
        where (nodes, restTokens) = isArrowTypes tokens
              hasError = hasAstError nodes
              astResult = createAstNode AstTypeDefinition [] nodes

    _isJsonKeyValue tokens =
        if hasError
        then (nodes, [])
        else ([astResult], restTokens)
        where (nodes, restTokens) = qExact [
                                              _isString
                                            , qOr [
                                                  isJsonArray
                                                , isJsonType
                                                , _isType]
                                            ] tokens
              hasError = hasAstError nodes
              astResult = createAstNode AstJsonKeyValueType [] nodes

    isJsonArray :: AstFn
    isJsonArray tokens =
        if hasError
        then (nodes, [])
        else ([astResult], restTokens)
        where (nodes, restTokens) = qExact [
                      _isOpenSquare
                    , qOr [_isType, isJsonArray, isJsonType]
                    , _isClosingSquare] tokens
              innerNodes = (init . tail) nodes
              hasError = hasAstError nodes
              astResult = createAstNode AstJsonArrayType [] innerNodes

    isJsonType :: AstFn
    isJsonType = withCurlyGroup AstJsonType [qZeroOrMore [_isJsonKeyValue]]

    --mTODO: isJson :: AstFn

    isClassFunction :: AstFn
    isClassFunction tokens =
        if hasError
        then (nodes, [])
        else ([astResult], restTokens)
        where (nodes, restTokens) = qExact [_isSymbol, isTypeDefinition] tokens
              hasError = hasAstError nodes
              astResult = createAstNode AstClassFunction [] nodes

    isClass :: AstFn
    isClass tokens = -- class Functor <T> { fmap <U> {T -> U} -> T -> U }
        if hasError
        then (nodes, [])
        else ([astResult], restTokens)
        where (nodes, restTokens) = qExact ([
                                              _hasTokenType T_ClassKeyword
                                            , _isType
                                            , isTemplateType
                                            , _isOpenCurly
                                            , qZeroOrMore [isClassFunction]
                                            , _isClosingCurly]) tokens
              hasError = hasAstError nodes
              astResult = createAstNode AstClass [] nodes

    -- TODO isClassInstance :: AstFn

    isPropKeyValueType :: AstFn -- a: Numbe
    isPropKeyValueType tokens =
        if hasError
        then (nodes, [])
        else ([astResult], restTokens)
        where (nodes, restTokens) = qExact [ _isProp, isTypeDefinitionWithoutTemplates ] tokens
              hasError = hasAstError nodes
              astResult = createAstNode AstPropKeyValueType [] nodes

    isPropListType :: AstFn -- [a: Numbe, b: String, c: imported.Type]
    isPropListType = withSquareGroup AstPropListType [qOneOrMore [isPropKeyValueType]]

    -- TODO: isPropList :: AstFn -- [a: 3 b: "5" c: void]

    isIfThenElse :: AstFn -- (if a then b else c)
    isIfThenElse = withRoundGroup AstIfCondition [
          _hasTokenType T_If
        , qOr [
              _isSymbol
            , _isBool
            , isFunctionCall
            ]
        , _hasTokenType T_Then
        , x
        , _hasTokenType T_Else
        , x
        ]
        where x = qOr [
                        _isSymbol
                      , _isPrimitive
                      , isFunctionCall
                      , isLambda
                      , isEnumValue
                      , isList
                      -- TODO: , isPropList
                      , isIfThenElse]

    -- TODO: isSwitch :: AstFn -- (switch a (2 "two") (5 "five") otherwise "wrong number")

    _isImport' :: AstFn -- import (a b c) from "./FeatureA.team"
    _isImport' tokens =
        if hasError
        then (nodes, [])
        else ([astResult], restTokens)
        where (nodes, restTokens) = qExact [
                                          _hasTokenType T_Import
                                        , _isOpenRound
                                        , qZeroOrMore [_isSymbol]
                                        , _isClosingRound
                                        , _hasTokenType T_From
                                        , _isString
                                        ] tokens
              hasError = hasAstError nodes
              astResult = createAstNode AstImport [] nodes

    _isImportAs :: AstFn -- import as a from "./FeatureA.team"
    _isImportAs tokens =
        if hasError
        then (nodes, [])
        else ([astResult], restTokens)
        where (nodes, restTokens) = qExact [
                                          _hasTokenType T_Import
                                        , _hasTokenType T_As
                                        , _isSymbol
                                        , _hasTokenType T_From
                                        , _isString
                                        ] tokens
              hasError = hasAstError nodes
              astResult = createAstNode AstImportAs [] nodes

    isImport :: AstFn
    isImport = qOr [_isImport', _isImportAs]

    isVar :: AstFn
    isVar = withRoundGroup AstVar [
                                      _hasTokenType T_Var
                                    , qOptional isTypeDefinition
                                    , _isSymbol
                                    , qOr [
                                            _isSymbol
                                        , _isPrimitive
                                        , isLambda
                                        , isEnumValue
                                        , isList
                                        , isFunctionCall
                                        , isIfThenElse
                                        -- TODO: , isPropList
                                        -- TODO: , isJson
                                        ]
                                    ]

    isTypeAlias :: AstFn
    isTypeAlias = withRoundGroup AstTypeAlias [
                                          _hasTokenType T_TypeKeyword
                                        , _isType'
                                        , isTypeDefinition
                                        ]

    isLet :: AstFn
    isLet = withRoundGroup AstLet [
                                      _hasTokenType T_Let
                                    , qOneOrMore [isTypeAlias, isVar, isEnum]
                                    , qOr [
                                            isFunctionCall
                                        , _isSymbol
                                        , isLambda
                                        , isIfThenElse
                                        , isList
                                        -- TODO: , isPropList
                                        -- TODO: , isJson
                                        ]
                                    ]

    -- TODO: isProp -- (prop x a: 0 "key" "key2" 0)
    -- TODO: isPropList
    -- TODO: isJson
    -- TODO: isSwitch
    -- TODO: isDO

    -- TODO: isFeature :: AstFn
    -- TODO: isProject :: AstFn
    -- TODO: isHotfix :: AstFn
    -- TODO: isUtil :: AstFn
    -- TODO: isConfig :: AstFn
    -- TODO: isPrototype :: AstFn
    -- TODO: isTBD :: AstFn
    -- TODO: isDeprecated :: AstFn
    -- TODO: isPlugin :: AstFn

    -- TODO: isTest :: AstFn

    -- TODO: isExport :: AstFn

    -- TODO: isTopLevel
