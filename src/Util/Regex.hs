{-# LANGUAGE OverloadedStrings #-}

module Util.Regex (
      matchRegex
    ) where

    import Text.Regex.Base.RegexLike
    import Text.Regex.TDFA.ByteString.Lazy
    import qualified Data.ByteString.Lazy.Char8 as L

    _regex_ :: L.ByteString -> Regex
    _regex_ x = (makeRegex (x :: L.ByteString)) :: Regex

    matchRegex :: L.ByteString -> L.ByteString -> Bool
    matchRegex r = matchTest (_regex_ r)
