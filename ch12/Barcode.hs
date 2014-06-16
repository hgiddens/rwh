import Control.Applicative ((<$>), (<*>))
import Control.Monad (forM_, replicateM)
import Data.Array (Array(..), (!), bounds, elems, indices, ixmap, listArray)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Char (digitToInt)
import Data.Ix (Ix(..))
import Data.List (foldl', group, sort, sortBy, tails)
import qualified Data.Map as M
import Data.Maybe (catMaybes, listToMaybe)
import Data.Ratio (Ratio)
import Data.Word (Word8)
import Parse
import System.Environment (getArgs)

checkDigit :: (Integral a) => [a] -> a
checkDigit ds = 10 - (sum products `mod` 10)
    where products = mapEveryOther (*3) (reverse ds)

mapEveryOther :: (a -> a) -> [a] -> [a]
mapEveryOther f = zipWith ($) (cycle [f,id])

leftOddList = ["0001101", "0011001", "0010011", "0111101", "0100011",
               "0110001", "0101111", "0111011", "0110111", "0001011"]
rightList = map complement <$> leftOddList
    where complement '0' = '1'
          complement '1' = '0'

leftEvenList = map reverse rightList

parityList = ["111111", "110100", "110010", "110001", "101100",
              "100110", "100011", "101010", "101001", "100101"]

listToArray :: [a] -> Array Int a
listToArray xs = listArray (0,l - 1) xs
    where l = length xs

leftOddCodes, leftEvenCodes, rightCodes, parityCodes :: Array Int String
leftOddCodes = listToArray leftOddList
leftEvenCodes = listToArray leftEvenList
rightCodes = listToArray rightList
parityCodes = listToArray parityList

foldA :: Ix k => (a -> b -> a) -> a -> Array k b -> a
foldA f s a = go s (indices a)
    where go s (j:js) = let s' = f s (a ! j)
                        in s' `seq` go s' js
          go s _ = s

foldA1 :: Ix k => (a -> a -> a) -> Array k a -> a
foldA1 f a = foldA f (a ! fst (bounds a)) a

encodeEAN13 :: String -> String
encodeEAN13 = concat . encodeDigits . map digitToInt

encodeDigits :: [Int] -> [String]
encodeDigits s@(first:rest) =
    [outerGuard] ++ lefties ++ [centerGuard] ++ righties ++ [outerGuard]
        where
          (left, right) = splitAt 5 rest
          lefties = zipWith leftEncode (parityCodes ! first) left
          righties = map rightEncode (right ++ [checkDigit s])

leftEncode :: Char -> Int -> String
leftEncode '1' = (leftOddCodes !)
leftEncode '0' = (leftEvenCodes !)

rightEncode :: Int -> String
rightEncode = (rightCodes !)

outerGuard = "101"
centerGuard = "01010"

type Pixel = Word8
type RGB = (Pixel, Pixel, Pixel)
type Pixmap = Array (Int, Int) RGB

parseRawPPM :: Parse Pixmap
parseRawPPM = do
    header <- parseWhileWith w2c (/= '\n')
    skipSpaces
    assert (header == "P6") "invalid raw header"
    width <- parseNat
    skipSpaces
    height <- parseNat
    skipSpaces
    maxValue <- parseNat
    assert (maxValue == 255) "max value out of spec"
    parseByte
    pxs <- parseTimes (width * height) parseRGB
    return $ listArray ((0,0), (width-1, height-1)) pxs

parseRGB :: Parse RGB
parseRGB = (,,) <$> parseByte <*> parseByte <*> parseByte

parseTimes :: Int -> Parse a -> Parse [a]
parseTimes = replicateM

luminance :: (Pixel, Pixel, Pixel) -> Pixel
luminance (r,g,b) = round $ r' * 0.3 + g' * 0.59 + b' * 0.11
    where
      r' = fromIntegral r
      g' = fromIntegral g
      b' = fromIntegral b

type Greymap = Array (Int, Int) Pixel

pixmapToGreymap :: Pixmap -> Greymap
pixmapToGreymap = fmap luminance
