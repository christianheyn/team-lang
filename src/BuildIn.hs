{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module BuildIn (

    ) where

buildIn = [
      "let"
    , "var"
    , "type"
    , "lens"
    , "alias"

    , "true"
    , "false"

    , "if"
    , "cond"
    , "do"

    , "parallel"
    , "concurrent"

    -- Functions
    , "throw"
    , "and"
    , "or"
    , "some"

    , "default"

    , "head"
    , "last"
    , "tail"
    , "init"

    , "map"
    , "mapi"
    , "filter"
    , "reduce"
    , "equal"
    , "isString"
    , "isFloat"
    , "isComplex"
    , "isBoolean"
    , "isNumber"
    , "isFunction"
    , "isLens"
    ]
