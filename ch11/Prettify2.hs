module Prettify2 (Doc(..), (<>), char, double, empty, hcat, line, punctuate, text) where

import Data.List (intersperse)

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
           deriving (Eq, Show)

empty :: Doc
empty = Empty

char :: Char -> Doc
char = Char

text :: String -> Doc
text "" = Empty
text s = Text s

line :: Doc
line = Line

double :: Double -> Doc
double = Text . show

(<>) :: Doc -> Doc -> Doc
Empty <> r = r
l <> Empty = l
l <> r = Concat l r

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

hcat :: [Doc] -> Doc
hcat = fold (<>)

punctuate :: Doc -> [Doc] -> [Doc]
punctuate _ [] = []
punctuate _ [d] = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds
