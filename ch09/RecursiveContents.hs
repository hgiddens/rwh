module RecursiveContents (getRecursiveContents) where

import Control.Monad (forM, mapM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))

ifM :: (Monad m) => m Bool -> m a -> m a -> m a
ifM c t f = c >>= (\x -> if x then t else f)

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topDir = do
  names <- getDirectoryContents topDir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
                                let path = topDir </> name
                                ifM (doesDirectoryExist path)
                                    (getRecursiveContents path)
                                    (return [path])
  return $ concat paths
