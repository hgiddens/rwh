{-# LANGUAGE RankNTypes #-}
module Glob (namesMatching) where

import Control.Exception (Exception, IOException, handle)
import Control.Monad (forM)
import Data.List (isSuffixOf)
import GlobRegex (matchesGlob)
import System.Directory (doesDirectoryExist, doesFileExist,
                         getCurrentDirectory, getDirectoryContents)
import System.FilePath (dropTrailingPathSeparator, splitFileName, (</>))
import System.Posix.Files (fileExist)

isPattern :: String -> Bool
isPattern = any (`elem` "[*?")

namesMatching pat
    | not (isPattern pat) =
        do exists <- fileExist pat
           return (if exists then [pat] else [])
    | otherwise =
        case splitFileName pat of
          ("", baseName) ->
              do curDir <- getCurrentDirectory
                 listMatches curDir baseName
          (dirName, baseName) ->
              do dirs <- dirnames dirName
                 let listDir = if isPattern baseName
                               then listMatches
                               else listPlain
                 pathNames <- forM dirs $ \dir ->
                               do baseNames <- listDir dir baseName
                                  return (map (dir </>) baseNames)
                 return (concat pathNames)

dirnames :: String -> IO [String]
dirnames dirName
    | "/**/" `isSuffixOf` dirName =
        let up = dropTrailingPathSeparator . fst . splitFileName
            dir = up dirName
        in do a <- namesMatching $ up dir
              b <- namesMatching (dir ++ "*")
              c <- namesMatching (dir ++ "*/**")
              return $ a ++ b ++ c
    | isPattern dirName = namesMatching (dropTrailingPathSeparator dirName)
    | otherwise = return [dirName]

listMatches :: FilePath -> String -> IO [String]
listMatches dirName pat =
    do dirName' <- if null dirName
                  then getCurrentDirectory
                  else return dirName
       handle errorHandler $ do 
         names <- getDirectoryContents dirName'
         let names' = if isHidden pat
                      then filter isHidden names
                      else filter (not . isHidden) names
         return $ filter matches names'
    where
      errorHandler :: IOException -> IO [a]
      errorHandler _ = return []
      -- exercise is beyond stupid
      matches name = matchesGlob True name pat

isHidden ('.':_) = True
isHidden _ = False

listPlain :: FilePath -> String -> IO [String]
listPlain dirName baseName = do exists <- if null baseName
                                         then doesDirectoryExist dirName
                                         else fileExist (dirName </> baseName)
                                return $ if exists then [baseName] else []
