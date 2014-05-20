import Control.Applicative ((<$>), (<*>), Alternative, empty, liftA2)
import Control.Category ((>>>))
import Control.Exception (IOException, bracket, handle)
import Control.Monad (filterM, forM, liftM)
import Data.Time.Clock (UTCTime(..))
import System.Directory (Permissions(..), getDirectoryContents, getModificationTime, getPermissions)
import System.FilePath ((</>), takeExtension)
import System.IO (IOMode(..), hClose, hFileSize, openFile)

import RecursiveContents (getRecursiveContents)

data Info = Info {
      infoPath :: FilePath,
      infoPerms :: Maybe FileMode,
      infoSize :: Maybe Integer,
      infoModTime :: Maybe UTCTime
    } deriving (Eq, Ord, Show)

data Iterate seed = Done { unwrap :: seed }
                  | Skip { unwrap :: seed }
                  | Continue { unwrap :: seed }
                    deriving (Show)
type Iterator seed = seed -> Info -> Iterate seed

foldTree :: Iterator a -> a -> FilePath -> IO a
foldTree iter initSeed path = unwrap <$> fold initSeed path
    where
      fold seed subpath = getUsefulContents subpath >>= walk seed subpath
      walk seed path (name:names) = do
        let path' = path </> name
        info <- getInfo path'
        case iter seed info of
          done@(Done _) -> return done
          Skip seed' -> walk seed' path names
          Continue seed'
            | isDirectory info ->
                do next <- fold seed' path'
                   case next of
                     done@(Done _) -> return done
                     seed'' -> walk (unwrap seed'') path names
            | otherwise -> walk seed' path names
      walk seed _ [] = return (Continue seed)

-- postorder
foldTree' :: Iterator a -> a -> FilePath -> IO a
foldTree' iter initSeed path = unwrap <$> fold initSeed path
    where
      fold seed subpath = getUsefulContents subpath >>= walk seed subpath
      walk seed path (name:names) = do
        let path' = path </> name
        info <- getInfo path' -- IO Info
        seed' <- if isDirectory info
                then do next <- fold seed path'
                        case next of
                          done@(Done _) -> return done
                          seed'' -> return $ iter (unwrap seed'') info
                else return $ iter seed info
        walk (unwrap seed') path names
      walk seed _ [] = return $ Continue seed

getInfo :: FilePath -> IO Info
getInfo path = do
  perms <- maybeIO $ getPermissions path
  size <- maybeIO $ bracket (openFile path ReadMode) hClose hFileSize
  modified <- maybeIO $ getModificationTime path
  return $ Info path perms size modified
    where
      maybeIO = handle ignoreError . liftM Just

traverse :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverse order path = do names <- getUsefulContents path
                         contents <- mapM getInfo (path : map (path </>) names)
                         liftM concat $ forM (order contents) $ \info ->
                           if isDirectory info && infoPath info /= path
                           then traverse order (infoPath info)
                           else return [info]

isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms

getUsefulContents :: FilePath -> IO [String]
getUsefulContents pth = do
  names <- getDirectoryContents pth
  return $ filter (`notElem` [".", ".."]) names

traverse' :: ([Info] -> [Info]) -> (Info -> Bool) -> FilePath -> IO [Info]
traverse' conv pred = traverse order
    where order = conv . filter pred
                    

ignoreError :: (Alternative f, Monad m) => IOException -> m (f a)
ignoreError = const $ return empty

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handle ignoreError $
                     bracket (openFile path ReadMode) hClose $ \h -> do
                       size <- hFileSize h
                       return $ Just size

simpleFileSize :: FilePath -> IO Integer
simpleFileSize path = do h <- openFile path ReadMode
                         size <- hFileSize h
                         hClose h
                         return size

saferFileSize :: FilePath -> IO (Maybe Integer)
saferFileSize path = handle ignoreError $ do
                       h <- openFile path ReadMode
                       size <- hFileSize h
                       hClose h
                       return $ Just size

betterFind :: (Info -> Bool) -> FilePath -> IO [FilePath]
betterFind p path = getRecursiveContents path >>= filterM check
    where
      check :: FilePath -> IO Bool
      check fp = undefined

constP :: a -> Info -> a
--constP k _ _ _ _ = k
constP = return

liftP :: (a -> b -> c) -> (Info -> a) -> b -> Info -> c
--liftP q f k w x y z = f w x y z `q` k
--liftP q f k w x y z = f w x y z `q` constP k w x y z
liftP f a b = liftP2 f a (constP b)

liftP2 :: (a -> b -> c) -> (Info -> a) -> (Info -> b) -> Info -> c
--liftP2 q f g w x y z = f w x y z `q` g w x y z
liftP2 = liftA2

liftPath :: (FilePath -> a) -> Info -> a
--liftPath f w _ _ _ = f w
liftPath = (>>>) infoPath

pathP :: Info -> FilePath
pathP = infoPath

sizeP :: Info -> Integer
--sizeP _ _ (Just size) _ = size
--sizeP _ _ _ _ = -1
sizeP = fmap (maybe (-1) id) infoSize

equalP, (==?) :: Eq a => (Info -> a) -> a -> (Info -> Bool)
--equalP f k = \w x y z -> f w x y z == k
--equalP f k w x y z = f w x y z == k
equalP = liftP (==)
(==?) = equalP
infix 4 ==?

greaterP, lesserP, (>?) :: (Ord a) => (Info -> a) -> a -> (Info -> Bool)
greaterP = liftP (>)
(>?) = greaterP
infix 4 >?
lesserP = liftP (<)

--simpleAndP :: InfoP Bool -> InfoP Bool -> InfoP Bool
--simpleAndP f g w x y z = f w x y z && g w x y z
andP, orP, (&&?) :: (Info -> Bool) -> (Info -> Bool) -> (Info -> Bool)
andP = liftP2 (&&)
(&&?) = andP
infixr 3 &&?
orP = liftP2 (||)

myTest :: Info -> Bool
--myTest path _ (Just size) _ = takeExtension path == ".cpp" && size > 131072
--myTest _ _ _ _ = False
myTest = liftPath takeExtension ==? ".cpp" &&? sizeP >? 131072

logVisits :: Iterator [FilePath]
logVisits seed info = Continue $ (infoPath info):seed
