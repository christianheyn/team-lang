{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module AST2 (
      AST(..)
    , AST_NODE_TYPE(..)
    , AST_NODE(..)
    , _string
    , __keyword
    , ___naturalNumber
    , _number
    , _complexNumber
    , _primitive
    , _comment
    , __symbol
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

    -- qOneOrMore :: [AstFn] -> AstFn
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
        if (isAstError ast)
        then qOr cs chars
        else (ast, rest)
        where (ast, rest) = c chars

    qJustAppear :: AstFn -> AstFn
    qJustAppear check chars =
        if (isAstError ast)
        then (ast, rest)
        else (AST_VALUE [], rest)
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

    __keyword :: L.ByteString -> L.ByteString -> (L.ByteString, L.ByteString)
    __keyword kw ""    = ("", "")
    __keyword kw chars = if (kw `L.isPrefixOf` chars) && (fstRest `L.elem` " (){}[]\n,;")
                         then (kw, justRest)
                         else ("", chars)
        where rest = L.stripPrefix kw chars
              fstRest = if (isJust rest && L.length (fromJust rest) /= 0)
                        then L.head (fromJust rest)
                        else ' '
              justRest = if (isJust rest)
                        then (fromJust rest)
                        else ""

    ___keyword :: L.ByteString -> AstFn
    ___keyword kw ""    = (endOfFileError, "")
    ___keyword kw chars = if (kw `L.isPrefixOf` chars) && (fstRest `L.elem` " (){}[]\n,;:")
                         then (AST_VALUE [], justRest)
                         else (AST_ERROR [], chars)
        where rest = L.stripPrefix kw chars
              fstRest = if (isJust rest && L.length (fromJust rest) /= 0)
                        then L.head (fromJust rest)
                        else ' '
              justRest = if (isJust rest)
                        then (fromJust rest)
                        else ""

    ___signAs :: AST_NODE_TYPE -> L.ByteString -> AstFn
    ___signAs t s chars =
        if L.head chars == L.head s
        then (singleAstNode t (Just s) (AST_VALUE []), L.tail chars)
        else unexpected chars

    -- SIGNS =============================================================

    ___imaginaryUnit :: AstFn
    ___imaginaryUnit = (___signAs AST_ImaginaryUnit "i")

    ___dot :: AstFn
    ___dot = ___signAs AST_Dot "."

    ___plus :: AstFn
    ___plus = ___signAs AST_Plus "+"

    ___minus :: AstFn
    ___minus = ___signAs AST_Minus "-"

    ___divide :: AstFn
    ___divide = ___signAs AST_Divide "/"

    ___sharp :: AstFn
    ___sharp = ___signAs AST_Comment "#"

    -- ()
    ___openRound :: AstFn
    ___openRound = ___signAs AST_Open "("

    ___closeRound :: AstFn
    ___closeRound = ___signAs AST_Close ")"

    -- {}
    ___openCurly :: AstFn
    ___openCurly = ___signAs AST_Open "{"

    ___closeCurly :: AstFn
    ___closeCurly = ___signAs AST_Close "}"

    -- []
    ___openSquare :: AstFn
    ___openSquare = ___signAs AST_Open "{"

    ___closeSquare :: AstFn
    ___closeSquare = ___signAs AST_Close "}"

    ___At :: AstFn
    ___At = ___signAs AST_At "@"

    __lookForward :: AstFn -> AstFn
    __lookForward check chars =
        if (isAstError ast)
        then (AST_ERROR [], chars)
        else (AST_VALUE [], chars)
        where (ast, rest) = check chars

    -- NUMBERS =============================================================
    ___naturalNumber :: AstFn -- 1, 2, 3 -- TODO: no zero at start
    ___naturalNumber ""    = (endOfFileError, "")
    ___naturalNumber chars =
        if L.length ns == 0
        then unexpected chars
        else (singleAstNode AST_NaturalNumber (Just ns) (AST_VALUE []), rest)
        where (ns, rest) = L.break (`L.notElem` "0123456789") chars

    ___integerNumber = -- -123
        (combinedAs AST_IntegerNumber)
        . qExact [qOptional ___minus, ___naturalNumber]

    ___rationalNumber = -- -2/3
        (wrappedAs AST_RationalNumber)
        . qExact [
              ___integerNumber
            , qJustAppear ___divide
            , ___naturalNumber]

    ___realNumber = -- -3.5
        (wrappedAs AST_RealNumber)
        . qExact [
              ___integerNumber
            , qJustAppear ___dot
            , ___naturalNumber]

    _complexNumber :: AstFn
    _complexNumber = (wrappedAs AST_ComplexNumber) . qExact [
          qOr [___realNumber, ___integerNumber]
        , qJustAppear ___plus
        , qOr [___realNumber, ___integerNumber] -- TODO: knowenError $
        , qJustAppear ___imaginaryUnit -- TODO: knowenError
        ]

    _number :: AstFn
    _number = (wrappedAs AST_Number) . qOr [
          ___realNumber
        , ___rationalNumber
        , ___integerNumber
        ]

    -- END NUMBERS =============================================================

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

    token check = qExact [
          check
        , qOptional $ qOr [_spaceEOF, _comment]
        , __lookForward xs ]
        where xs = qOr [
                          ___dot
                        , ___sharp
                        , ___openRound
                        , ___closeRound
                        , ___openCurly
                        , ___closeCurly
                        , ___openSquare
                        , ___closeSquare
                        ]

    _primitive = token $ qOr [
        _complexNumber
        , _number
        , _string
        ]

    __symbol :: L.ByteString -> (L.ByteString, L.ByteString)
    __symbol ""    = ("", "")
    __symbol chars =
        if L.length cs /= 0 && ((L.head chars) `L.notElem` notStart)
        then (cs, fromJust $ L.stripPrefix cs chars)
        else ("", chars)
        where cs = L.takeWhile (`L.notElem` notCs) chars
              notCs = "()[]{} \n,;:#\"." <> (L.pack ['A'..'Z'])
              notStart = (L.pack ['0'..'9'])
