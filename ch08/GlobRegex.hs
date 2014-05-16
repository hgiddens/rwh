module GlobRegex
    (globToRegex,
     matchesGlob) where

{- RegexLike instances
   Bool - match success
   Int - match count
   LHS string type - first match substring
   http://hackage.haskell.org/package/regex-base-0.93.2/docs/Text-Regex-Base-RegexLike.html
-}

import Text.Regex.Base.RegexLike (makeRegexOpts, match)
import Text.Regex.Posix ((=~), compBlank, compIgnoreCase, execBlank)

globToRegex :: String -> String
globToRegex cs = '^' : globToRegex' cs ++ "$"

globToRegex' :: String -> String
globToRegex' "" = ""
globToRegex' ('*':cs) = ".*" ++ globToRegex' cs
globToRegex' ('?':cs) = '.' : globToRegex' cs
globToRegex' ('[':'!':c:cs) = "[^" ++ c : charClass cs
globToRegex' ('[':c:cs) = '[' : c : charClass cs
globToRegex' ('[':_) = error "unterminated character class"
globToRegex' (c:cs) = escape c ++ globToRegex' cs

escape :: Char -> String
escape c | c `elem` regexChars = '\\' : [c]
         | otherwise = [c]
    where regexChars = "\\+()^$.{}]|"

charClass :: String -> String
charClass (']':cs) = ']' : globToRegex' cs
charClass (c:cs) = c : charClass cs
charClass _ = error "unterminated character class"

matchesGlob :: Bool -> String -> String -> Bool
matchesGlob foldCase name pattern = match (buildRegex $ globToRegex pattern) name
    where
      buildRegex = makeRegexOpts compOpts execBlank
      compOpts = if foldCase then compIgnoreCase else compBlank
