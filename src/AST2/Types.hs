{-# LANGUAGE OverloadedStrings #-}

module AST2.Types (
      AST(..)
    , AST_NODE_TYPE(..)
    , AST_NODE(..)
    , AstFn(..)
    , AstResult(..)
    , isAstValue
    , isAstError
    , isAstKnowenError
    , fromAST
    , combinedAs
    , wrappedAs
    ) where

    import qualified Data.ByteString.Lazy.Char8 as L
    import Data.List (find, null, or, break)
    import Data.Maybe (isJust, fromJust)

    data AST a =
          AST_VALUE a
        | AST_ERROR a
        | AST_KNOWEN_ERROR a
        deriving (Show, Eq)

    isAstValue :: AST a -> Bool
    isAstValue (AST_VALUE _) = True
    isAstValue _             = False

    isAstError ( AST_ERROR _)        = True
    isAstError _                     = False

    isAstKnowenError ( AST_KNOWEN_ERROR _) = True
    isAstKnowenError _                     = False

    fromAST (AST_VALUE x)        = x
    fromAST (AST_ERROR x)        = x
    fromAST (AST_KNOWEN_ERROR x) = x

    instance Functor AST where
        fmap f ast =
            if (isAstKnowenError ast)
            then AST_KNOWEN_ERROR (f $ fromAST ast)
            else if (isAstError ast)
                 then AST_ERROR (f $ fromAST ast)
                 else AST_VALUE (f $ fromAST ast)

    instance (Semigroup a) => Semigroup (AST a) where
        ast1 <> ast2 =
            if (isAstKnowenError ast1) || (isAstKnowenError ast2)
            then AST_KNOWEN_ERROR (fromAST ast1 <> fromAST ast2)
            else if (isAstError ast1) || (isAstError ast2)
                 then AST_ERROR (fromAST ast1 <> fromAST ast2)
                 else AST_VALUE (fromAST ast1 <> fromAST ast2)

    instance (Semigroup a, Monoid a) => Monoid (AST a) where
        mempty = AST_ERROR mempty
        mappend = (<>)
        mconcat = foldl1 (<>)

    data AST_NODE_TYPE =
        -- NUMBERS ============
          AST_Number
        |   AST_NaturalNumber  -- N ; 1 2 3 4 5 ...
        |   AST_IntegerNumber  -- Z ; -3
        |   AST_RealNumber     -- R ; -3.5
        |   AST_RationalNumber -- Q ; 2/3
        |   AST_ComplexNumber  -- C ; 2+4i
        |   AST_ImaginaryUnit  -- i
        |   AST_BinaryNumber   -- 2|010011
        |   AST_OctalNumber    -- 8|010011
        |   AST_HexNumber      -- 16|010011
        |   AST_PercentNumber  -- 2.5%

        | AST_Symbol         -- variable-name
        | AST_ImportedSymbol -- lib.fn
        | AST_PropSymbol     -- abc-123:

        | AST_IfStatement    -- if a then b else c

        | AST_TypeSymbol             -- T
            | AST_ImportedTypeSymbol -- lib.T
            | AST_TemplateType       -- <T>
            | AST_RestType           -- @T , @[Number]
            | AST_MaybeType          -- maybe Number
            | AST_EitherType         -- either Number String (T U)
            | AST_WrapType           -- A B C
            | AST_FunctionType       -- A -> B -> C D -> @F -> N
            | AST_TypeDefinition     -- <T> <U> {T -> U} [T] -> [U]
            | AST_PropListType       -- [a: Number, b: Number]

        | AST_PropListKeyValue   -- a: N, a: 2
        | AST_PropList           -- [ a: N, a: 2]
        | AST_JsonKeyValue       -- "key": value
        | AST_Json               -- { "key1": value "key2": value}
        | AST_String             -- "text"
        | AST_Char               -- 'c'
        | AST_Open               -- ( { [
        | AST_Close              -- ] } )
        | AST_At                 -- @
        | AST_Comment            -- # comment
        | AST_Coma               -- ,
        | AST_Semicolon          -- ;
        | AST_Space

        | AST_Function
        |   AST_Lambda
        |   AST_FunctionParameterList
        |   AST_FunctionBody
        |   AST_FunctionCall

        | AST_Syntax_Error
        | AST_Ignore
        | AST_Combination
        deriving (Show, Eq)

    data AST_NODE = AST_NODE {
          _astNodeType :: AST_NODE_TYPE
        , _astValue    :: Maybe L.ByteString
        , _astChildren :: AST [AST_NODE]
        } deriving (Show, Eq)

    instance Semigroup AST_NODE where
        ast1 <> ast2 = if (AST_Syntax_Error `elem` [_astNodeType ast1, _astNodeType ast2])
            then AST_NODE {
                    _astNodeType   = AST_Syntax_Error
                    , _astValue    = (_astValue ast1) <> (_astValue ast2)
                    , _astChildren = AST_ERROR []
                }
            else AST_NODE {
                    _astNodeType   = AST_Combination
                    , _astValue    = (_astValue ast1) <> (_astValue ast2)
                    , _astChildren = AST_VALUE []
                }

    instance Monoid AST_NODE where
        mempty = AST_NODE {
                    _astNodeType   = AST_Syntax_Error
                    , _astValue    = Nothing
                    , _astChildren = AST_ERROR []
                }
        mappend = (<>)
        mconcat = foldl1 (<>)

    type AstResult = (AST [AST_NODE], L.ByteString)

    type AstFn = L.ByteString -> AstResult

    combinedAs :: AST_NODE_TYPE -> AstResult -> AstResult
    combinedAs asType (AST_ERROR as, rest) = (AST_ERROR as, rest)
    combinedAs asType (AST_KNOWEN_ERROR as, rest) = (AST_KNOWEN_ERROR as, rest)
    combinedAs asType (ast, rest) = (fmap go ast, rest)
        where changeType newType ast_node = ast_node { _astNodeType = newType }
              go = (:[]) . (changeType asType) . mconcat

    combinedValue =
          _astValue
        . head
        . fromAST
        . fst
        . (combinedAs AST_Combination)

    wrappedAs :: AST_NODE_TYPE -> AstResult -> AstResult
    wrappedAs asType (AST_ERROR as, rest) = (AST_ERROR as, rest)
    wrappedAs asType (AST_KNOWEN_ERROR as, rest) = (AST_KNOWEN_ERROR as, rest)
    wrappedAs asType (ast, rest) = (newAst, rest)
        where newAst = AST_VALUE [
                        AST_NODE {
                            _astNodeType   = asType
                            , _astValue    = Nothing -- combinedValue (ast, rest)
                            , _astChildren = ast
                            }
                     ]

