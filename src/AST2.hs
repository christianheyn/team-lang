{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module AST2 (
      AST(..)
    , AST_NODE_TYPE(..)
    , AST_NODE(..)
    , AstResult(..)
    , fromAST
    , _string
    , string
    , _number
    , _complexNumber
    , primitive
    , symbol
    , allSymbols

    -- KEYWORDS
    , keywordIf
    , keywordThen
    , keywordElse
    , keywordMaybe
    , keywordEither

    -- TYPES
    , typeSymbol
    , allTypeSymbols
    , ___templateType
    , ___restType
    , ___maybeType
    , ___eitherType


    , _comment
    , funCurly
    , lambda
    , function
    , functionCall
    ) where


    import qualified Data.ByteString.Lazy.Char8 as L
    import Data.List (find, null, or, break)
    import Data.Char (isSpace)
    import Data.Maybe (isJust, fromJust)
    import AST2.Types

    singleAstNode t val children = AST_VALUE [
        AST_NODE {
          _astNodeType = t
        , _astValue    = val
        , _astChildren = children
        }
        ]

    unexpected chars = (ast, L.tail chars)
        where ast = AST_ERROR [
                        AST_NODE {
                              _astNodeType = AST_Syntax_Error
                            , _astValue    = Just (L.take 1 chars)
                            , _astChildren = AST_ERROR []
                            }
                        ]

    endOfFileError = AST_ERROR [
        AST_NODE {
            _astNodeType   = AST_Syntax_Error
            , _astValue    = Just "End of file"
            , _astChildren = AST_ERROR []
        }
        ]

    -- QUANTIFIER =============================================================

    qZeroOrMore :: AstFn -> AstFn
    qZeroOrMore check chars =
        if isAstError ast
        then (AST_VALUE [], chars)
        else (ast <> nextAst, nextRest)
        where (ast, rest) = check chars
              (nextAst, nextRest) = qZeroOrMore check rest

    qOneOrMore :: AstFn -> AstFn
    qOneOrMore check = qExact [check, qZeroOrMore check]

    qOptional :: AstFn -> AstFn
    qOptional _ ""        = (AST_VALUE [], "")
    qOptional check chars =
        if isAstError ast
        then (AST_VALUE [], chars)
        else (ast, rest)
        where (ast, rest) = check chars

    qExact :: [AstFn] -> AstFn
    qExact []     chars = (AST_VALUE [], chars)
    qExact (x:_) ""    = (AST_ERROR [], "")
    qExact (c:cs) chars =
        if (isAstError ast)
        then (ast, rest)
        else if (isAstError nextAst)
             then (AST_ERROR ( fromAST ast ++ fromAST nextAst), nextRest)
             else (AST_VALUE ( fromAST ast ++ fromAST nextAst), nextRest)
        where (ast, rest) = c chars
              (nextAst, nextRest) = qExact cs rest

    qOr :: [AstFn] -> AstFn
    qOr []    chars  = (AST_ERROR [], chars)
    qOr (x:_) ""     = (AST_ERROR [], "")
    qOr (c:cs) chars =
        if (isAstError ast) -- TODO: break up on isAstKnowenError
        then qOr cs chars
        else (ast, rest)
        where (ast, rest) = c chars

    qJustAppear :: AstFn -> AstFn
    qJustAppear check chars =
        if (isAstError ast)
        then (ast, rest)
        else (AST_VALUE [], rest)
        where (ast, rest) = check chars

    qlookForward :: AstFn -> AstFn
    qlookForward check chars =
        if (isAstError ast)
        then (AST_ERROR [], chars)
        else (AST_VALUE [], chars)
        where (ast, rest) = check chars

    qNegate :: AstFn -> AstFn
    qNegate check chars =
        if (isAstError ast)
        then (AST_VALUE (fromAST ast), rest)
        else (AST_ERROR (fromAST ast), rest)
        where (ast, rest) = check chars

    -- END QUANTIFIER =============================================================

    ___collectString :: L.ByteString -> (Maybe L.ByteString, L.ByteString)
    ___collectString ""       = (Nothing, "")
    ___collectString "\"\""   = (Just "", "")
    ___collectString chars    = if   L.head chars == '"'
                         then result
                         else (Nothing, chars)
        where (a, b)          = L.break (== '"') (L.tail chars)
              slashes         = L.takeWhile (== '\\') (L.reverse a)
              isShlashedQuote = (L.length slashes /= 0)
                                    && (odd $ L.length slashes)
              result          = if   isShlashedQuote
                                then if isJust nextA
                                     then (Just ( a <> "\"" <> (fromJust nextA)), nextB)
                                     else (Nothing, b)
                                else (Just a, L.tail b)
              (nextA, nextB)  = ___collectString b

    _string :: AstFn
    _string ""    = (endOfFileError, "")
    _string "\""  = (endOfFileError, "")
    _string chars = if (isJust str)
                     then (singleAstNode AST_String str (AST_VALUE []), rest)
                     else (AST_ERROR [], chars)
        where (str, rest) = ___collectString chars

    ___signAs :: AST_NODE_TYPE -> Char -> AstFn
    ___signAs t char chars =
        if L.head chars == char
        then (singleAstNode t (Just (L.pack (char:[]))) (AST_VALUE []), L.tail chars)
        else unexpected chars

    -- SIGNS =============================================================

    ___imaginaryUnit :: AstFn
    ___imaginaryUnit = (___signAs AST_ImaginaryUnit 'i')

    ___dot :: AstFn
    ___dot = ___signAs AST_Ignore '.'

    ___minus :: AstFn
    ___minus = ___signAs AST_Ignore '-'

    ___sharp :: AstFn
    ___sharp = ___signAs AST_Comment '#'

    -- ()
    ___openRound :: AstFn
    ___openRound = ___signAs AST_Open '('

    ___closeRound :: AstFn
    ___closeRound = ___signAs AST_Close ')'

    -- {}
    ___openCurly :: AstFn
    ___openCurly = ___signAs AST_Open '{'

    ___closeCurly :: AstFn
    ___closeCurly = ___signAs AST_Close '}'

    -- []
    ___openSquare :: AstFn
    ___openSquare = ___signAs AST_Open '{'

    ___closeSquare :: AstFn
    ___closeSquare = ___signAs AST_Close '}'

    -- []
    ___openTag :: AstFn
    ___openTag = ___signAs AST_Open '<'

    ___closeTag :: AstFn
    ___closeTag = ___signAs AST_Close '>'

    ___At :: AstFn
    ___At = ___signAs AST_At '@'

    ___Coma :: AstFn
    ___Coma = ___signAs AST_Coma ','

    ___Semicolon :: AstFn
    ___Semicolon = ___signAs AST_Semicolon ';'

    ___StringStart :: AstFn
    ___StringStart = ___signAs AST_String '"'

    withRoundGroup :: AstFn -> AstFn
    withRoundGroup check = qExact ([
              qJustAppear ___openRound
            , ignored
            ]
            ++ [check]
            ++ [
              ignored
            , qJustAppear ___closeRound
            , ignored
            ])

    withOptionalRoundGroup :: AstFn -> AstFn
    withOptionalRoundGroup check = qOr [withRoundGroup check, check]

    withCurlyGroup check = qExact ([
              qJustAppear ___openCurly
            , ignored
            ]
            ++ [check]
            ++ [
              ignored
            , qJustAppear ___closeCurly
            ])

    -- NUMBERS =============================================================
    ___naturalNumber :: AstFn -- 1, 2, 3 -- TODO: no zero at start
    ___naturalNumber ""    = (endOfFileError, "")
    ___naturalNumber chars =
        if L.length ns == 0
        then unexpected chars
        else (singleAstNode AST_NaturalNumber (Just ns) (AST_VALUE []), rest)
        where (ns, rest) = L.break (`L.notElem` "0123456789") chars

    ___integerNumber :: AstFn
    ___integerNumber = -- -123
        (combinedAs AST_IntegerNumber)
        . qExact [qOptional ___minus, ___naturalNumber]

    ___rationalNumber :: AstFn
    ___rationalNumber = -- -2/3
        (wrappedAs AST_RationalNumber)
        . qExact [
              ___integerNumber
            , qJustAppear (___signAs AST_Ignore '/')
            , ___naturalNumber]

    ___realNumber :: AstFn
    ___realNumber = -- -3.5
        (wrappedAs AST_RealNumber)
        . qExact [
              ___integerNumber
            , qJustAppear ___dot
            , ___naturalNumber]

    ___binaryNumber :: AstFn
    ___binaryNumber =
        (combinedAs AST_BinaryNumber) . qExact [
              qJustAppear $ ___signAs AST_Ignore '2'
            , qJustAppear $ ___signAs AST_Ignore '|'
            , qOptional $ ___minus
            , qOneOrMore $ qOr [
                  ___signAs AST_NaturalNumber '0'
                , ___signAs AST_NaturalNumber '1'
                ]
            ]

    ___octalNumber :: AstFn
    ___octalNumber =
        (combinedAs AST_OctalNumber) . qExact [
              qJustAppear $ ___signAs AST_Ignore '8'
            , qJustAppear $ ___signAs AST_Ignore '|'
            , qOptional $ ___minus
            , qOneOrMore $
                qOr $ fmap (___signAs AST_NaturalNumber) ['0'..'7']
            ]

    ___hexNumber :: AstFn
    ___hexNumber =
        (combinedAs AST_HexNumber) . qExact [
              qJustAppear $ ___signAs AST_Ignore '1'
            , qJustAppear $ ___signAs AST_Ignore '6'
            , qJustAppear $ ___signAs AST_Ignore '|'
            , qOptional $ ___minus
            , qOneOrMore $
                qOr $ fmap (___signAs AST_NaturalNumber) (['0'..'9'] ++ ['a'..'f'])
            ]

    ___percentNumber = (wrappedAs AST_PercentNumber) . qExact [
              qOr [___realNumber, ___integerNumber]
            , qJustAppear $ ___signAs AST_Ignore '%'
            ]

    _complexNumber :: AstFn
    _complexNumber = (wrappedAs AST_ComplexNumber) . qExact [
          qOr [___realNumber, ___integerNumber]
        , qJustAppear (___signAs AST_Ignore '+')
        , qOr [___realNumber, ___integerNumber] -- TODO: knowenError $
        , qJustAppear (___signAs AST_Ignore 'i') -- TODO: knowenError
        ]

    _number :: AstFn -- TODO: Improvement with ___stepsFuntion
                     -- because when a realNumber has an error after the .
                     -- it is a interger number; or when a percentNumber
                     -- has no percent sign everything before is a number
    _number = (wrappedAs AST_Number) . qOr [
          ___percentNumber
        , ___realNumber
        , ___binaryNumber
        , ___octalNumber
        , ___hexNumber
        , ___rationalNumber
        , ___integerNumber
        ]

    number = token $ _number
    complexNumber = token $ _complexNumber

    -- END NUMBERS =============================================================

    -- SYMBOLS =================================================================

    ___symbol :: AstFn -- abc123
    ___symbol chars =
        if L.length chars == 0
        then (endOfFileError, "")
        else if wrongStart
             then unexpected chars
             else (singleAstNode AST_Symbol (Just xs) (AST_VALUE []), rest)
        where (xs, rest) = L.break (notAllowed) chars
              notAllowed c = c `L.elem` (L.pack ("@#(){}[]\" \n." ++ ['A'..'Z']))
              wrongStart = (L.head chars) `L.elem` notAllowedStart
              notAllowedStart = L.pack ("@#(){}[]\" \n." ++ ['A'..'Z'] ++ ['0'..'9'])

    symbol :: AstFn
    symbol = token $ ___symbol

    importedSymbol :: AstFn
    importedSymbol = token
        $ (wrappedAs AST_ImportedSymbol)
            . qExact [___symbol, qJustAppear ___dot, ___symbol]

    allSymbols :: AstFn
    allSymbols = qOr [importedSymbol, symbol]

    -- END SYMBOLS =============================================================

    -- KEYWORDS  ===============================================================

    ___keyword :: L.ByteString -> AstFn
    ___keyword k chars =
        if isAstError ast
        then (ast, rest)
        else if n == Just k
             then (AST_VALUE [], rest)
             else unexpected chars
        where (ast, rest) = symbol chars
              n = _astValue $ head $ fromAST ast

    keywordIf         = ___keyword "if"
    keywordThen       = ___keyword "then"
    keywordElse       = ___keyword "else"
    keywordSwitch     = ___keyword "switch"
    keywordOtherwise  = ___keyword "otherwise"
    keywordMaybe      = ___keyword "maybe"
    keywordEither     = ___keyword "either"
    keywordVar        = ___keyword "var"
    keywordType       = ___keyword "type"
    keywordLet        = ___keyword "let"
    keywordDo         = ___keyword "do"
    keywordCatch      = ___keyword "catch"
    keywordImport     = ___keyword "import"
    keywordAs         = ___keyword "as"
    keywordFrom       = ___keyword "from"
    keywordExport     = ___keyword "export"
    keywordClass      = ___keyword "class"
    keywordInstance   = ___keyword "instance"
    keywordLens       = ___keyword "lens"
    keywordFocus      = ___keyword "focus"
    keywordXml        = ___keyword "xml"
    keywordArrowLeft  = ___keyword "->"
    keywordConcurrent = ___keyword "concurrent"
    keywordParallel   = ___keyword "parallel"
    keywordTest       = ___keyword "test"
    keywordFun        = ___keyword "fun"

    -- TYPING ==================================================================

    ___typeSymbol :: AstFn -- abc123
    ___typeSymbol chars =
        if L.length chars == 0
        then (endOfFileError, "")
        else if L.head chars `L.notElem` (L.pack ['A'..'Z'])
             then unexpected chars
             else (singleAstNode AST_TypeSymbol (Just xs) (AST_VALUE []), rest)
        where (xs, rest) = L.break (notAllowed) chars
              notAllowed c = c `L.notElem` allowedBody
              allowedBody = (L.pack (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ['_']))

    typeSymbol :: AstFn
    typeSymbol = token $ ___typeSymbol

    importedTypeSymbol = token
        $ (wrappedAs AST_ImportedTypeSymbol)
            . qExact [___symbol, qJustAppear ___dot, ___typeSymbol]

    allTypeSymbols :: AstFn
    allTypeSymbols = withOptionalRoundGroup
                        $ qOr [importedTypeSymbol, typeSymbol]

    ___templateType :: AstFn
    ___templateType = (wrappedAs AST_TemplateType) . qExact [
          qJustAppear ___openTag
        , ignored
        , typeSymbol
        , qJustAppear ___closeTag
        , ignored
        ]

    ___maybeType = (wrappedAs AST_MaybeType)
                    . withOptionalRoundGroup
                        (qExact [keywordMaybe, ignored, all])
        where all = qOr [
                      ___maybeType
                    , ___eitherType
                    --TODO: , ___functionTypeDef
                    --TODO: , ___listType
                    --TODO: , ___propListType
                    , allTypeSymbols
                    ]

    ___eitherType = (wrappedAs AST_EitherType)
                    . withOptionalRoundGroup
                        (qExact [
                              keywordEither
                            , ignored
                            , all
                            , ignored
                            , all
                            , ignored
                            , qZeroOrMore all
                            , ignored ])
        where all = qOr [
                      ___maybeType
                    --TODO: , ___functionTypeDef
                    --TODO: , ___listType
                    --TODO: , ___propListType
                    , allTypeSymbols
                    ]


    ___allTypes :: AstFn
    ___allTypes = qOr [
        allTypeSymbols
        ]

    ___restType :: AstFn
    ___restType = (wrappedAs AST_RestType) . qExact [
          qJustAppear ___At
        , ignored
        , ___allTypes
        ]

    -- END TYPING ============================================================

    __commentText :: AstFn
    __commentText chars =
        (singleAstNode AST_Comment (Just ns) (AST_VALUE []), rest)
        where (ns, rest) = L.break (== '\n') chars

    _comment :: AstFn
    _comment = (combinedAs AST_Comment) . qExact [___sharp, __commentText]

    _spaceEOF :: AstFn
    _spaceEOF ""    = (AST_VALUE [], "")
    _spaceEOF chars =
        if L.length ns == 0
        then (AST_ERROR [], rest)
        else (singleAstNode AST_Space (Just ns) (AST_VALUE []), rest)
        where (ns, rest) = L.break (not . isSpace) chars

    ignored = qJustAppear $ qZeroOrMore (qOr [_spaceEOF, _comment])
    coma = qExact [qJustAppear $ qOptional ___Coma, ignored]

    -- FUNCTION ===============================================================

    ___functionParameterList :: AstFn
    ___functionParameterList =
        (wrappedAs AST_FunctionParameterList) . qExact [
              qJustAppear ___openRound
            , ignored
            , qZeroOrMore $ qExact [symbol, coma]
            , ignored
            , qJustAppear ___closeRound
            ]

    ___functionBody :: AstFn
    ___functionBody = (wrappedAs AST_FunctionBody) . qOr [
                            primitive
                          , functionCall
                          , lambda
                          -- TODO: , switch
                          -- TODO: , ifThenElse
                          , allSymbols
                          ]

    lambda :: AstFn
    lambda =
        token $
            (wrappedAs AST_Lambda)
            . withCurlyGroup (qExact [
            -- TODO: , qOptional typing
              ___functionParameterList
            , ignored
            , ___functionBody
            , ignored
            -- TODO: , qOptional catch
            ])

    funCurly :: AstFn
    funCurly =
        token $
            (wrappedAs AST_Function)
            . withCurlyGroup (qExact [
              symbol
            , ignored
            -- TODO: , qOptional typing
            , ___functionParameterList
            , ignored
            , ___functionBody
            , ignored
            -- TODO: , qOptional catch
            -- TODO: , ignored
            ])

    fun :: AstFn
    fun =
        token $
            (wrappedAs AST_Function)
            . (qExact [
                keywordFun
                , symbol
                -- TODO: , qOptional typing
                , ___functionParameterList
                , ignored
                , ___functionBody
                , ignored
                -- TODO: , qOptional catch
                ])

    function = qOr [fun, funCurly]

    -- FUNCTIONCALL ===============================================================

    functionCall :: AstFn
    functionCall = token $
            (wrappedAs AST_FunctionCall)
            . withRoundGroup (qExact [
                symbol
              , coma
              , qZeroOrMore $ qExact [x, coma]
            ])
        where x = qOr [
                      primitive
                    , allSymbols
                    , lambda
                    , functionCall
                    ]


    token check = qExact [
          check
        , qOr [
              qlookForward xs
            , ignored
            ]
        ]
        where
              xs = qOr [
                          ___dot
                        , ___sharp
                        , ___openRound
                        , ___closeRound
                        , ___openCurly
                        , ___closeCurly
                        , ___openSquare
                        , ___closeSquare
                        , ___Coma
                        , ___Semicolon
                        , ___StringStart
                        ]

    string = qExact [
          _string
        , ignored
        ]

    primitive = qOr [
          complexNumber
        , number
        , string
        -- , char
        -- , true
        -- , false
        -- , pi
        -- , e
        ]

