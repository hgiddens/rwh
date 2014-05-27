import Control.Applicative (Alternative, (<|>), empty, pure)
import Control.Arrow (first, second)
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Char (isSpace)
import Greymap (Greymap(..))

whenA, unlessA :: Alternative f => a -> Bool -> f a
whenA f b = if b then pure f else empty
unlessA f b = whenA f (not b)

parseP5 :: L.ByteString -> Maybe (Greymap, L.ByteString)
parseP5 s = do
  s <- matchHeader (L8.pack "p5") s
  (_, s) <- skipSpace ((), s)
  (width, s) <- getNat s >>= skipSpace
  (height, s) <- getNat s >>= skipSpace
  (maxGrey, s) <- getNat s
  () `unlessA` (maxGrey > 255)
  (_, s) <- getBytes 1 s
  (bitmap, s) <- getBytes (width * height) s
  return (Greymap width height maxGrey bitmap, s)

matchHeader :: L.ByteString -> L.ByteString -> Maybe L.ByteString
matchHeader prefix str =
    L8.dropWhile isSpace (L.drop (L.length prefix) str) `whenA` (prefix `L8.isPrefixOf` str)

getNat :: L.ByteString -> Maybe (Int, L.ByteString)
getNat = fmap (first fromIntegral) . L8.readInt

getBytes :: Int -> L.ByteString -> Maybe (L.ByteString, L.ByteString)
getBytes n str = let count = fromIntegral n
                     both@(prefix, _) = L.splitAt count str
                 in both `unlessA` (L.length prefix < count)

skipSpace :: (a, L.ByteString) -> Maybe (a, L.ByteString)
skipSpace = return . second (L8.dropWhile isSpace)
