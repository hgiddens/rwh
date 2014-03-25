-- file: ch03/ListADT.hs
data List a = Cons a (List a)
            | Nil
              deriving (Show)

fromList (x:xs) = Cons x (fromList xs)
fromList []     = Nil

toList (Cons a l) = a:(toList l)
toList Nil = []

data JTree a = JTree a (Maybe (JTree a)) (Maybe (JTree a))
               deriving (Show)

singleton x = JTree x Nothing Nothing

tadd x t@(JTree v l r) = insert $ compare x v
    where
      insert EQ = t
      insert LT = JTree v (insert' l) r
      insert GT = JTree v l (insert' r)

      insert' Nothing = Just $ singleton x
      insert' (Just t') = Just $ tadd x t'
