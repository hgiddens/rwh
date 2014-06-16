{-# LANGUAGE TemplateHaskell #-}

import Control.Monad (liftM, liftM2)
import Data.List (intersperse)
import Data.Monoid (mappend, mempty)
import Prettify2
--import Test.Framework (defaultMain, testGroup)
--import Test.Framework.Providers.QuickCheck
import Test.QuickCheck

instance Arbitrary Doc where
    arbitrary = do
      oneof [ return Empty,
              liftM Char arbitrary,
              liftM Text arbitrary,
              return Line,
              liftM2 Concat arbitrary arbitrary,
              liftM2 Union arbitrary arbitrary]

prop_empty_id x = empty <> x == x && x <> empty == x
prop_char c = char c == Char c
prop_text s = text s == if null s then Empty else Text s
prop_line = line == Line
prop_double d = double d == text (show d)

prop_hcat xs = hcat xs == glue xs
    where
      glue [] = empty
      glue (d:ds) = d <> glue ds
prop_punctuate s xs = punctuate s xs == combine (intersperse s xs)
    where
      combine [] = []
      combine [x] = [x]
      combine (x:Empty:xs) = x:combine xs
      combine (Empty:x:xs) = x:combine xs
      combine (x:y:xs) = x `Concat` y : combine xs

prop_mappend :: Doc -> Doc -> Bool
prop_mappend x y = x `mappend` y == x <> y

prop_mempty_id :: Doc -> Bool
prop_mempty_id x = x `mappend` mempty == x && mempty `mappend` x == x

runTests = $quickCheckAll

main = runTests
