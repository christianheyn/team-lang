{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module AST2 (
      AST_NODE_TYPE(..)
    , AST_NODE(..)
    , __string
    ) where

    import qualified Data.ByteString.Lazy.Char8 as L
    import Data.List (find, null, or, break)
    import Data.Maybe (isJust, fromJust)

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
        | AstEitherType         -- either T U V {W -> U}
        | AstFunctionType       -- {T -> U}
        | AstListType           -- [T]
        | AstJsonType           -- { "key" Value "key2" { "key3" String } "key4" [Number] }
        | AstJsonKeyValueType   -- "key" Value
        | AstJsonArrayType      -- [T]
        | AstClassFunction      -- fmap <U> {T -> U} -> [T] -> [U]
        | AstClass              -- class Functor <T> { fmap <U> {T -> U} -> [T] -> [U] }
        | AstClassInstance      -- instance Functor F { {fmap (f x) (f x)} }
        | AstProp               -- a:
        | AstPropKeyValueType   -- a: Number
        | AstPropListType       -- [a: Number]
        | AstPropKeyValue       -- [a: Number]
        | AstPropList           -- [a: Number]
        | AstPairType           -- (Number , String)
        | AstTripleType         -- (Number , String, CNumber)
        | AstJsonKeyValue
        | AstJson
        | AstPair               -- pair 1 "2"
        | AstTriple             -- triple 1 "2" E:e
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
        | AstVar                -- var a CNumber = 6+4i)
        | AstComp               -- comp f = f3 f2 f1
        | AstPipe               -- pipe f = f1 f2 f3
        | AstLet                -- (let (type T [Number]) (var Number a 3) (print a))
        | AstSwitchValue
        | AstSwitch
        | AstOtherwiseValue
        | AstOpen               -- ({[
        | AstClose              -- ]})
        | AstImport
        | AstImportAs
        | AstError
        deriving (Show, Eq)

    data AST_NODE = AST_NODE {
          _astNodeType :: AST_NODE_TYPE
        , _astTokens   :: [L.ByteString]
        , _astChildren :: [AST_NODE]
        } deriving (Show, Eq)

    type AstFn = L.ByteString -> ([AST_NODE], L.ByteString)


    -- QUANTIFIER =============================================================

    -- qOneOrMore :: [AstFn] -> AstFn
    -- qOneOrMore :: [AstFn] -> AstFn
    -- qOptional  :: AstFn -> AstFn
    -- qExact     :: [AstFn] -> AstFn
    -- qOr        :: [AstFn] -> AstFn

    -- END QUANTIFIER =============================================================

    __string :: L.ByteString -> (L.ByteString, L.ByteString)
    __string ""    = ("", "") -- TODO: empty strings
    __string chars = if   L.head chars == '"'
                     then result
                     else ("", chars)
        where (a, b)          = L.break (== '"') (L.tail chars)
              slashes         = L.takeWhile (== '\\') (L.reverse a)
              isShlashedQuote = (L.length slashes /= 0)
                                    && (odd $ L.length slashes)
              result          = if   isShlashedQuote
                                then (( a <> "\"" <> a'), b')
                                else (a, L.tail b)
              (a', b')        = __string b
