{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module AST2 (
      AST(..)
    , AST_NODE_TYPE(..)
    , AST_NODE(..)
    , __string
    , __keyword
    , ___naturalNumber
    , ___number
    , __symbol
    ) where

    import qualified Data.ByteString.Lazy.Char8 as L
    import Data.List (find, null, or, break)
    import Data.Maybe (isJust, fromJust)

    data AST a =
          AST_VALUE a
        | AST_ERROR a
         deriving (Show, Eq)

    isAstValue (AST_VALUE _) = True
    isAstValue _             = False

    isAstError ( AST_ERROR _) = True
    isAstError _              = False

    fromAST (AST_VALUE x)  = x
    fromAST ( AST_ERROR x) = x

    data AST_NODE_TYPE =
          AST_Number -- 1 2 3 4 5 ...
        | AST_Minus         -- -
        | AST_Plus          -- +
        | AST_Divide        -- /
        | AST_Dot           -- .
        | AST_String        -- "text"
        | AST_Syntax_Error
        | AST_Ignore
        deriving (Show, Eq)

    data AST_NODE = AST_NODE {
          _astNodeType :: AST_NODE_TYPE
        , _astValue    :: Maybe L.ByteString
        , _astChildren :: AST [AST_NODE]
        } deriving (Show, Eq)

    type AstFn = L.ByteString -> (AST [AST_NODE], L.ByteString)

    -- combinedAs :: AST_NODE_TYPE -> AstFn -> AstFn

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
    qOr []     chars = (AST_ERROR [], chars)
    qOr (x:_) ""    = (AST_ERROR [], "")
    qOr (c:cs) chars =
        if (isAstError ast)
        then qOr cs rest
        else (ast, rest)
        where (ast, rest) = c chars

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

    __string :: AstFn
    __string ""    = (endOfFileError, "")
    __string "\""  = (endOfFileError, "")
    __string chars = if (isJust str)
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
    ___keyword kw chars = if (kw `L.isPrefixOf` chars) && (fstRest `L.elem` " (){}[]\n,;")
                         then (AST_VALUE [], justRest)
                         else (AST_ERROR [], chars)
        where rest = L.stripPrefix kw chars
              fstRest = if (isJust rest && L.length (fromJust rest) /= 0)
                        then L.head (fromJust rest)
                        else ' '
              justRest = if (isJust rest)
                        then (fromJust rest)
                        else ""

    -- NUMBERS =============================================================
    ___signAs :: AST_NODE_TYPE -> L.ByteString -> AstFn
    ___signAs t s chars =
        if L.head chars == L.head s
        then (singleAstNode t (Just s) (AST_VALUE []), L.tail chars)
        else unexpected chars

    ___dot :: AstFn
    ___dot = ___signAs AST_Dot "."

    ___plus :: AstFn
    ___plus = ___signAs AST_Plus "+"

    ___minus :: AstFn
    ___minus = ___signAs AST_Minus "-"

    ___divide :: AstFn
    ___divide = ___signAs AST_Divide "/"

    ___naturalNumber :: AstFn
    ___naturalNumber ""    = (endOfFileError, "")
    ___naturalNumber chars =
        if L.length ns == 0
        then unexpected chars
        else (singleAstNode AST_Number (Just ns) (AST_VALUE []), rest)
        where (ns, rest) = L.break (`L.notElem` "0123456789") chars

    ___number = qExact [
          qOptional ___minus
        , ___naturalNumber
        , qOptional $ qOr [
              qExact [___dot, ___naturalNumber]
            , qExact [___divide, ___naturalNumber]
            ]
        ]

    -- END NUMBERS =============================================================

    __symbol :: L.ByteString -> (L.ByteString, L.ByteString)
    __symbol ""    = ("", "")
    __symbol chars = if L.length cs /= 0
                        && ((L.head chars) `L.notElem` notStart)
                     then (cs, fromJust $ L.stripPrefix cs chars)
                     else ("", chars)
        where cs = L.takeWhile (`L.notElem` notCs) chars
              notCs = "()[]{} \n,;:#\"."
              notStart = (L.pack ['0'..'9']) <> (L.pack ['A'..'Z'])

