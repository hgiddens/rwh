import Control.Applicative (Alternative, (<|>), empty, pure)
import Control.Arrow (first)
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Char (isSpace)
import Text.Printf (printf)

data Greymap = Greymap {
      greyWidth :: Int,
      greyHeight :: Int,
      greyMax :: Int,
      greyData :: L.ByteString
    } deriving (Eq)

instance Show Greymap where
    show (Greymap w h m _) = printf "Greymap %dx%d %d" w h m

whenA, unlessA :: Alternative f => a -> Bool -> f a
whenA f b = if b then pure f else empty
unlessA f b = whenA f (not b)

parseP5 :: L.ByteString -> Maybe (Greymap, L.ByteString)
parseP5 s = do
  s1 <- matchHeader (L8.pack "p5") s
  (width, s2) <- getNat s1
  (height, s3) <- getNat (L8.dropWhile isSpace s2)
  (maxGrey, s4) <- getNat (L8.dropWhile isSpace s3)
  () `unlessA` (maxGrey > 255)
  (_, s5) <- getBytes 1 s4
  (bitmap, s6) <- getBytes (width * height) s5
  return (Greymap width height maxGrey bitmap, s6)

matchHeader :: L.ByteString -> L.ByteString -> Maybe L.ByteString
matchHeader prefix str =
    L8.dropWhile isSpace (L.drop (L.length prefix) str) `whenA` (prefix `L8.isPrefixOf` str)

getNat :: L.ByteString -> Maybe (Int, L.ByteString)
getNat = fmap (first fromIntegral) . L8.readInt

getBytes :: Int -> L.ByteString -> Maybe (L.ByteString, L.ByteString)
getBytes n str = let count = fromIntegral n
                     both@(prefix, _) = L.splitAt count str
                 in both `unlessA` (L.length prefix < count)
