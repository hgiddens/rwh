import Data.Char (digitToInt)

safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead _ = Nothing

safeTail :: [a] -> Maybe [a]
safeTail (_:t) = Just t
safeTail _ = Nothing

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast [x] = Just x
safeLast (_:t) = safeLast t

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit as = Just (safeInit' as)
    where
      safeInit' [_] = []
      safeInit' (h:t) = h:(safeInit' t)

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith p [] = []
splitWith p as = case break p as of
                   ([], []) -> []
                   ([], (_:t)) -> splitWith p t
                   (h, []) -> h:[]
                   (h, (_:t)) -> h:(splitWith p t)

firstWords :: String -> [String]
firstWords input = concatMap firstWord $ lines input
    where
      firstWord line = case words line of
                         (h:_) -> [h]
                         _ -> []

transposeText :: String -> String
transposeText = unlines . transpose . lines
    where
      transpose [] = []
      transpose as = if any null as
                     then []
                     else (map head as):(transpose $ map tail as)

asInt_fold :: String -> Int
asInt_fold ('-':t) = -1 * asInt_fold t
asInt_fold t = asInt_fold' t
    where
      asInt_fold' = foldl (\i c -> (i * 10) + (d2i c)) 0
      d2i i
          | i >= '0' && i <= '9' = (fromEnum i) - (fromEnum '0')
          | otherwise = error "Nope"

asInt_either :: String -> Either String Int
asInt_either ('-':t) = fmap negate (asInt_either t)
asInt_either t = asInt_either' t
    where
      asInt_either' = foldl step (Right 0)
      step acc c = do
        i <- acc
        cc <- d2i c
        return $ (i * 10) + cc
      d2i i
          | i >= '0' && i <= '9' = Right $ (fromEnum i) - (fromEnum '0')
          | otherwise = Left "Nope"

my_concat :: [[a]] -> [a]
my_concat = foldr (++) []

my_takeWhile :: (a -> Bool) -> [a] -> [a]
my_takeWhile _ [] = []
my_takeWhile p (h:t)
    | p h = h:(my_takeWhile p t)
    | otherwise = my_takeWhile p t

my_takeWhile_foldR :: (a -> Bool) -> [a] -> [a]
my_takeWhile_foldR p = foldr step []
    where
      step v = if (p v) then (v:) else id

my_groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
my_groupBy f as = foldr step [] as
    where
      step v [] = [[v]]
      step v (hs@(h:_):r) = if (f v h) then (v:hs):r else [v]:hs:r

my_any :: (a -> Bool) -> [a] -> Bool
my_any p = foldr (\a b -> (p a) || b) False
