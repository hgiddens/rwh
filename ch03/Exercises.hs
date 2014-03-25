myLength :: (Num b) => [a] -> b
myLength (x:xs) = (1+) $ myLength xs
myLength [] = 0

mean xs = (sum xs) / (myLength xs)

palindrome (x:xs) = [x] ++ (palindrome xs) ++ [x]
palindrome [] = []

isPalindrome xs = (reverse xs) == xs

sort _ [] = []
sort _ [x] = [x]
sort cmp (h:xs) = (sort cmp $ lt xs) ++ [h] ++ (sort cmp $ ge xs)
    where
      lt = filter (\x -> cmp x h)
      ge = filter (\x -> not (cmp x h))

swizzle :: [[a]] -> [[a]]
swizzle = sort $ (<) `on` length

intersperse :: a -> [[a]] -> [a]
intersperse s [] = []
intersperse s [a] = a
intersperse s (a:as) = a ++ [s] ++ (intersperse s as)

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

height :: (Integral i) => Tree a -> i
height Empty = 0
height (Node _ l r) = succ $ max (height l) (height r)

data Direction = LeftTurn | Straight | RightTurn
               deriving (Show)

turn (ax,ay) (bx,by) (cx,cy) = convert cross
    where
      cross = ((bx-ax)*(cy-ay)) - ((by-ay)*(cx-ax))
      convert x
          | x < 0 = RightTurn
          | x > 0 = LeftTurn
          | otherwise = Straight

turns = (map turn') . split
    where
      split (a:b:c:t) = (a,b,c):(split (b:c:t))
      split _ = []
      turn' (a,b,c) = turn a b c

on :: (b -> b -> c) -> (a -> b) -> (a -> a -> c)
on f g = \m n -> f (g m) (g n)

