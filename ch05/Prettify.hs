module Prettify
    (Doc,
     (<>),
     char,
     compact,
     double,
     fsep,
     hcat,
     pretty,
     punctuate,
     series,
     string,
     text) where
         
import Data.Bits ((.&.), shiftR)
import Data.Char (ord)
import Numeric (showHex)

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
           deriving (Eq, Show)

string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar

text :: String -> Doc
text "" = Empty
text s = Text s

double :: Double -> Doc
double = text . show

enclose :: Char -> Char -> Doc -> Doc
enclose left right x = char left <> x <> char right

(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
x <> y = Concat x y

char :: Char -> Doc
char = Char

hcat :: [Doc] -> Doc
hcat = fold (<>)

oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of
              Just r -> text r
              Nothing | mustEscape c -> hexEscape c
                      | otherwise -> char c
    where
      mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
    where ch a b = (a, ['\\', b])

smallHex :: Int -> Doc
smallHex x = text "\\u" <> text zeroes <> text h
    where h = showHex x ""
          zeroes = replicate (4 - length h) '0'

astral :: Int -> Doc
astral n = smallHex (a + 0xd800) <> smallHex (b + 0xdc00)
    where a = (n `shiftR` 10) .&. 0x3ff
          b = n .&. 0x3ff

hexEscape :: Char -> Doc
hexEscape c | d < 0x10000 = smallHex d
            | otherwise = astral (d - 0x10000)
    where d = ord c

series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close item = enclose open close
                         . fsep
                         . punctuate (char ',')
                         . map item

fsep :: [Doc] -> Doc
fsep = fold (</>)

punctuate :: Doc -> [Doc] -> [Doc]
punctuate _ [] = []
punctuate _ [d] = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f Empty

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y
    where
      softline = Union (Char ' ') Line

-- softline :: Doc
-- softline = group Line
-- 
-- group :: Doc -> Doc
-- group x = flatten x `Union` x
-- 
-- flatten :: Doc -> Doc
-- flatten (Concat x y) = Concat (flatten x) (flatten y)
-- flatten Line = Char ' '
-- flatten (Union x _) = flatten x
-- flatten other = other

compact :: Doc -> String
compact doc = transform [doc]
    where
      transform [] = ""
      transform (d:ds) =
          case d of
            Empty -> transform ds
            Char c -> c:transform ds
            Text s -> s ++ transform ds
            Line -> '\n':transform ds
            Concat a b -> transform (a:b:ds)
            Union _ b -> transform (b:ds)

pretty :: Int -> Doc -> String
pretty width x = best 0 [x]
    where best col (d:ds) =
              case d of
                Empty -> best col ds
                Char c -> c : best (col + 1) ds
                Text s -> s ++ best (col + length s) ds
                Line -> '\n' : best 0 ds
                Concat a b -> best col (a:b:ds)
                Union a b -> nicest col (best col (a:ds))
                                       (best col (b:ds))
          best _ _ = ""
          nicest col a b | (width - least) `fits` a = a
                         | otherwise = b
                         where least = min width col

fits :: Int -> String -> Bool
fits w _ | w < 0 = False
fits w "" = True
fits w ('\n':_) = True
fits w (c:cs) = fits (w - 1) cs

pad :: Int -> Int -> Doc -> Doc
pad width col doc = doc <> text (replicate (max 0 (width - col)) '_')

fill :: Int -> Doc -> Doc
fill width doc = uncurry (pad width) $ fill' 0 doc
    where
      fill' :: Int -> Doc -> (Int, Doc)
      fill' col Empty = (col, Empty)
      fill' col c@(Char _) = (col + 1, c)
      fill' col t@(Text s) = (col + length s, t)
      fill' col Line = (0, pad width col Empty <> Line)
      fill' col (Concat a b) = let (col', a') = fill' col a
                                   (col'', b') = fill' col' b
                               in (col'', Concat a' b')
      fill' col (Union _ b) = fill' col b
