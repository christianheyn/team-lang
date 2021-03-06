{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Tokenizer (
      _isString
    , _isUnresolvedString
    , _endingChars
    , _isNumber
    , _isUnresolvedNumber
    , _isComplexNumber
    , _isUnresolvedComplexNumber
    , _isType
    , _isSymbol
    , _isProp
    , _isEnumMember
    , _isReferenceDot
    , _isRestSpread
    , _isOpenSquareBracket
    , _isClosingSquareBracket
    , _isOpenCurlyBracket
    , _isClosingCurlyBracket
    , _isOpenRoundBracket
    , _isClosingRoundBracket
    , _isComment
    , _isSpace
    , _isNewline
    , _isSeparator
    , tokenize
    , generateTokens
    , TokenType(..)
    , Token(..)
    ) where

    import GHC.Generics
    import Data.Data (Typeable)
    import Util.Regex
    import qualified Data.ByteString.Lazy.Char8 as L
    import Control.Lens

    _endingChars :: Char -> L.ByteString -> Int
    _endingChars c str = length $ go str
        where group x y = (x == '\\' && y == c) || (x == '\\' && y == '\\')
              group' = filter (c `L.elem`)
              evenGroup = filter (even . L.length . L.init)
              go = evenGroup . group' . (L.groupBy group)

    _makeIsUnresolvedWrapper_ :: Char -> Char -> L.ByteString -> Bool
    _makeIsUnresolvedWrapper_ h l str
        | str == ""              = False
        | str == (L.pack [h, l]) = False
        | otherwise = and [L.head str == h , endings == 0]
            where endings = _endingChars l (L.tail str)

    _isUnresolvedString :: L.ByteString -> Bool
    _isUnresolvedString "" = False
    _isUnresolvedString x  = _makeIsUnresolvedWrapper_ '"' '"' x

    _isString :: L.ByteString -> Bool
    _isString ""     = False
    _isString "\"\"" = True
    _isString x      = and [
                          not $ _isUnresolvedString x
                        , _isUnresolvedString (L.init x)
                        , (L.last x ) == '"'
                        ]

    _senslessZero_ = matchRegex "^(\\-?)0{1}([0-9]{1,})"
    _noSenslessZero_ = not . _senslessZero_

    _naturalReg = "(\\-|\\+?)([0-9]{1,})(\\.{1}([0-9]{1,})){0,1}"
    _rationlReg = "(\\-|\\+)?([1-9]{1}[0-9]*)(\\/){1}(\\-|\\+)?([1-9]{1}[0-9]*)"
    _complexReg = _naturalReg <> "(\\+)" <> _naturalReg <> "\\i"

    _isNumber :: L.ByteString -> Bool
    _isNumber ""  = False
    _isNumber "0" = True
    _isNumber "1" = True
    _isNumber x   = _noNewlineStart x && _noSenslessZero_ x && match x
        where match = matchRegex (
                                    "^(("
                                    <> _naturalReg
                                    <> ")|("
                                    <> _rationlReg
                                    <> "))$"
                                 )

    _isUnresolvedNumber :: L.ByteString -> Bool
    _isUnresolvedNumber "" = False
    _isUnresolvedNumber x  = _noNewlineStart x && _noSenslessZero_ x && match x
        where match = matchRegex "^((\\-?)([0-9]{1,})(\\.{1}|\\/{1}))$"

    _isComplexNumber :: L.ByteString -> Bool
    _isComplexNumber ""  = False
    _isComplexNumber x   = _noNewlineStart x && _noSenslessZero_ x && match x
        where match = matchRegex ("^(" <> _complexReg <> ")$")

    _isUnresolvedComplexNumber :: L.ByteString -> Bool
    _isUnresolvedComplexNumber "" = False
    _isUnresolvedComplexNumber x  = _noNewlineStart x && _noSenslessZero_ x && match x
        where match = matchRegex (
                                    "^((\\-?)([0-9]{1,})(\\.{1}|\\/{1})|("
                                    <> _complexReg'
                                    <> ")|("
                                    <> _complexReg''
                                    <> ")|("
                                    <> _complexReg'''
                                    <> ")|("
                                    <> _complexReg''''
                                    <> "))$"
                                 )
              _complexReg'    = _naturalReg <> "(\\+)"
              _complexReg''   = _naturalReg <> "(\\+)" <> _naturalReg
              _complexReg'''  = _naturalReg <> "(\\+)" <> "(\\-|\\+)?"
              _complexReg'''' = _naturalReg <> "(\\+)" <> "(\\-|\\+)?([0-9]+\\.)"

    _isType :: L.ByteString -> Bool
    _isType "" = False
    _isType x  = _noNewlineStart x && matchRegex "^([A-Z]{1})([0-9A-Za-z_])*$" x

    _isOpenSquareBracket :: L.ByteString -> Bool
    _isOpenSquareBracket "[" = True
    _isOpenSquareBracket _   = False

    _isClosingSquareBracket :: L.ByteString -> Bool
    _isClosingSquareBracket "]" = True
    _isClosingSquareBracket _   = False

    _isOpenCurlyBracket :: L.ByteString -> Bool
    _isOpenCurlyBracket "{" = True
    _isOpenCurlyBracket _   = False

    _isClosingCurlyBracket :: L.ByteString -> Bool
    _isClosingCurlyBracket "}" = True
    _isClosingCurlyBracket _   = False

    _isOpenRoundBracket :: L.ByteString -> Bool
    _isOpenRoundBracket "(" = True
    _isOpenRoundBracket _   = False

    _isClosingRoundBracket :: L.ByteString -> Bool
    _isClosingRoundBracket ")" = True
    _isClosingRoundBracket _   = False

    _noNewlineStart x = L.head x /= '\n' && L.last x /= '\n'

    _isComment :: L.ByteString -> Bool
    _isComment "" = False
    _isComment x  = _noNewlineStart x && matchRegex "^#.*$" x

    _isSymbol :: L.ByteString -> Bool
    _isSymbol "" = False
    _isSymbol x  = _noNewlineStart x && matchRegex ("^(" <> reg1 <> "|" <> reg2 <> ")(')*$") x
        where reg1 = "([-a-z_\\=\\~\\&\\|\\*\\+\\<\\>\\/\\?\\!\\$\\%\\^]+[-a-z_0-9\\=\\~\\&\\|\\*\\+\\<\\>\\/\\?\\!\\$\\%\\^]*)"
              reg2 = "([-a-z_]+[-a-z_A-Z0-9]*)"

    _isProp :: L.ByteString -> Bool
    _isProp "" = False
    _isProp x  = _noNewlineStart x && matchRegex "^[a-z_]+[-a-z_A-Z0-9]*(\\:){1}$" x

    _isEnumMember :: L.ByteString -> Bool
    _isEnumMember "" = False
    _isEnumMember x  = _noNewlineStart x && matchRegex "^(\\:){1}[-a-z_A-Z0-9]*$" x

    _isReferenceDot :: L.ByteString -> Bool
    _isReferenceDot "" = False
    _isReferenceDot x  = _noNewlineStart x && matchRegex "^(\\.){1}$" x

    _isRestSpread :: L.ByteString -> Bool
    _isRestSpread "" = False
    _isRestSpread x  = _noNewlineStart x && matchRegex "^(\\@){1}$" x

    _isNewline :: L.ByteString -> Bool
    _isNewline x  = x == "\n"

    _isSpace :: L.ByteString -> Bool
    _isSpace x  = _noNewlineStart x && matchRegex "^( )+$" x

    _isSeparator :: L.ByteString -> Bool
    _isSeparator "," = True
    _isSeparator _   = False

    _isSemicolon :: L.ByteString -> Bool
    _isSemicolon ";" = True
    _isSemicolon _   = False

    data TokenType =
          T_Symbol
        | T_Prop
        | T_EnumMember
        | T_Type
        | T_MaybeType
        | T_EitherType
        | T_Number
        | T_ComplexNumber
        | T_String
        | T_BooleanTrue
        | T_BooleanFalse
        | T_Void
        | T_ReferenceDot
        | T_RestSpread

        | T_OpenSquareBracket
        | T_ClosingSquareBracket

        | T_OpenCurlyBracket
        | T_ClosingCurlyBracket

        | T_OpenRoundBracket
        | T_ClosingRoundBracket

        | T_Comment
        | T_Space
        | T_Newline
        | T_Separator
        | T_Semicolon

        -- step 2
        | T_EqualSign
        | T_Export
        | T_Import
        | T_As
        | T_From
        | T_Let
        | T_Var
        | T_PropKeyword
        | T_TypeKeyword
        | T_ClassKeyword
        | T_ClassInstance
        | T_EnumKeyword
        | T_Do
        | T_If
        | T_Then
        | T_Else
        | T_Catch
        | T_Test
        | T_Parallel
        | T_Concurrent
        | T_ArrowLeft
        | T_Switch
        | T_FatArrowLeft
        | T_Otherwise
        | T_CompositionKeyword
        | T_PipeKeyword
        | T_Pair
        | T_Triple
        | T_JsonNull

        | T_FlagFeature
        | T_FlagProject
        | T_FlagHotfix
        | T_FlagPlugin
        | T_FlagFramework
        | T_FlagUtil
        | T_FlagConfig
        | T_FlagPrototype
        | T_FlagTBD
        | T_FlagDeprecated

        | T_FunctionAdditionKeyword
        | T_ModuleStrucKeyword
        | T_Unknowen
        deriving (Show, Eq, Generic, Typeable)

    type TokenValidator = ((L.ByteString -> Bool), TokenType)

    untilTrue :: [TokenValidator] -> L.ByteString -> Bool
    untilTrue [] _     = False
    untilTrue (f:fs) x = if ((fst f) x)
                         then True
                         else untilTrue fs x

    -- step 2
    predicateToType = [
          (_isNewline,                 T_Newline)
        , (_isSpace,                   T_Space)

        , (_isType,                    T_Type)
        , (_isSymbol,                  T_Symbol)
        , (_isProp,                    T_Prop)
        , (_isEnumMember,              T_EnumMember)
        , (_isReferenceDot,            T_ReferenceDot)
        , (_isRestSpread,              T_RestSpread)

        , (_isOpenSquareBracket,       T_OpenSquareBracket)
        , (_isClosingSquareBracket,    T_ClosingSquareBracket)

        , (_isOpenCurlyBracket,        T_OpenCurlyBracket)
        , (_isClosingCurlyBracket,     T_ClosingCurlyBracket)

        , (_isOpenRoundBracket,        T_OpenRoundBracket)
        , (_isClosingRoundBracket,     T_ClosingRoundBracket)

        , (_isUnresolvedNumber,        T_Number)
        , (_isNumber,                  T_Number)

        , (_isUnresolvedComplexNumber, T_ComplexNumber)
        , (_isComplexNumber,           T_ComplexNumber)

        , (_isString,                  T_String)
        , (_isUnresolvedString,        T_String)

        , (_isComment,                 T_Comment)
        , (_isSemicolon,               T_Semicolon)
        , (_isSeparator,               T_Separator)
        ]

    _combineToken_ ::
           L.ByteString
        -> [L.ByteString]
        -> [L.ByteString]
    _combineToken_ "" []   = []
    _combineToken_ "" acc  = acc
    _combineToken_ _ []    = []
    _combineToken_ src acc =
        if   (untilTrue predicateToType token)
        then _combineToken_ src' (init acc ++ [token])
        else _combineToken_ src' (acc ++ [e])
        where h     = L.head src
              l     = last acc
              e     = L.pack [h]
              token = L.append l e
              src'  = if L.length src == 0
                      then ""
                      else L.tail src

    tokenize :: L.ByteString -> [L.ByteString]
    tokenize src = _combineToken_ src [""]

    data Token = Token {
          _TType :: TokenType
        , _TValue :: L.ByteString
        , _TIndex :: Int
        } deriving (Show, Eq, Generic, Typeable)
    makeLenses ''Token

    _untilType_ :: [TokenValidator] -> L.ByteString -> TokenType
    _untilType_ [] _     = T_Unknowen
    _untilType_ (f:fs) x = if ((fst f) x)
                           then (snd f)
                           else _untilType_ fs x

    specialSymbolsPredicat = [
              ((== "pair"), T_Pair)
            , ((== "triple"), T_Triple)
            , ((== "pi"), T_Number)
            , ((== "e"), T_Number)
            , ((== "="), T_EqualSign)
            , ((== "if"), T_If)
            , ((== "then"), T_Then)
            , ((== "else"), T_Else)
            , ((== "let"), T_Let)
            , ((== "var"), T_Var)
            , ((== "prop"), T_PropKeyword)
            , ((== "type"), T_TypeKeyword)
            , ((== "<<<"), T_CompositionKeyword)
            , ((== ">>>"), T_PipeKeyword)
            , ((== "class"), T_ClassKeyword)
            , ((== "instance"), T_ClassInstance)
            , ((== "enum"), T_EnumKeyword)
            , ((== "test"), T_Test)
            , ((== "export"), T_Export)
            , ((== "import"), T_Import)
            , ((== "as"), T_As)
            , ((== "from"), T_From)
            , ((== "maybe"), T_MaybeType)
            , ((== "either"), T_EitherType)
            , ((== "catch"), T_Catch)
            , ((== "do"), T_Do)
            , ((== "parallel"), T_Parallel)
            , ((== "concurrent"), T_Concurrent)
            , ((== "true"), T_BooleanTrue)
            , ((== "false"), T_BooleanFalse)
            , ((== "void"), T_Void)
            , ((== "null"), T_JsonNull)
            , ((== "->"), T_ArrowLeft)
            , ((== "switch"), T_Switch)
            , ((== "=>"), T_FatArrowLeft)
            , ((== "otherwise"), T_Otherwise)
            , ((== "feature"), T_FlagFeature)
            , ((== "project"), T_FlagProject)
            , ((== "hotfix"), T_FlagHotfix)
            , ((== "plugin"), T_FlagPlugin)
            , ((== "framework"), T_FlagFramework)
            , ((== "util"), T_FlagUtil)
            , ((== "config"), T_FlagConfig)
            , ((== "prototype"), T_FlagPrototype)
            , ((== "tbd"), T_FlagTBD)
            , ((== "deprecated"), T_FlagDeprecated)

            ]
            ++ predicateToType
            ++ [((\_ -> True), T_Unknowen)]

    generateAllTokens :: L.ByteString -> [Token]
    generateAllTokens = (go 0) . tokenize
        where go :: Int -> [L.ByteString] -> [Token]
              go _     []     = []
              go index (t:ts) = [
                  Token {
                        _TType  = _untilType_ specialSymbolsPredicat t
                      , _TValue = t
                      , _TIndex = index
                  }
                ] ++ (go (index + 1) ts)

    generateTokens :: L.ByteString -> [Token]
    generateTokens = (filter onlyExecutable) . generateAllTokens
        where onlyExecutable t = and [
                  _TType t /= T_Space
                , _TType t /= T_Newline
                , _TType t /= T_Comment
                , _TType t /= T_Separator
                , _TType t /= T_Semicolon
                ]
