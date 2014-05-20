module GlobRegexEither
    (globToRegex,
     matchesGlob) where

import Text.Regex.Base.RegexLike (makeRegexOpts, match)
import Text.Regex.Posix ((=~), compBlank, compIgnoreCase, execBlank)

type GlobError = String

globToRegex :: String -> Either GlobError String
globToRegex cs = do regex <- globToRegex' cs
                    return $ "^" ++ regex ++ "$"

globToRegex' :: String -> Either GlobError String
globToRegex' "" = return ""
globToRegex' ('*':cs) = fmap (".*" ++) (globToRegex' cs)
globToRegex' ('?':cs) = fmap ('.' :) (globToRegex' cs)
globToRegex' ('[':'!':c:cs) = do klass <- charClass cs
                                 return $ "[^" ++ c : klass
globToRegex' ('[':c:cs) = do klass <- charClass cs
                             return $ '[' : c : klass
globToRegex' ('[':_) = Left "unterminated character class"
globToRegex' (c:cs) = fmap (escape c ++) (globToRegex' cs)

escape :: Char -> String
escape c | c `elem` regexChars = '\\' : [c]
         | otherwise = [c]
    where regexChars = "\\+()^$.{}]|"

charClass :: String -> Either GlobError String
charClass (']':cs) = fmap (']':) (globToRegex' cs)
charClass (c:cs) = fmap (c:) (charClass cs)
charClass _ = Left $ "unterminated character class"

matchesGlob :: Bool -> String -> String -> Either GlobError Bool
matchesGlob foldCase name pattern = do regex <- globToRegex pattern
                                       return $ match (buildRegex regex) name
    where
      buildRegex = makeRegexOpts compOpts execBlank
      compOpts = if foldCase then compIgnoreCase else compBlank
