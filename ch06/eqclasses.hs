class BasicEq a where
    isEqual :: a -> a -> Bool

instance BasicEq Bool where
    isEqual True True = True
    isEqual False False = True
    isEqual _ _ = False

class BasicEq3 a where
    isEqual3, isNotEqual3 :: a -> a -> Bool
    isEqual3 = isNotEqual3
    isNotEqual3 = isEqual3

data Color = Red | Green | Blue

instance BasicEq3 Color where
    isEqual3 Red Red = True
    isEqual3 Green Green = True
    isEqual3 Blue Blue = True
    isEqual3 _ _ = False

instance Show Color where
    show Red = "Red"
    show Green = "Green"
    show Blue = "Blue"

instance Read Color where
    readsPrec _ value = tryParse [("Red", Red), ("Green", Green), ("Blue", Blue)]
        where
          tryParse [] = []
          tryParse ((attempt, result):rest) =
              if (take (length attempt) value) == attempt
              then [(result, drop (length attempt) value)]
              else tryParse rest

data CannotShow = CannotShow
                  deriving (Show)

data CannotDeriveShow = CannotDeriveShow CannotShow
                        deriving (Show)

class Foo a where
    asFoo :: a -> String

instance Foo Bool where
    asFoo b = "Foo Bool: " ++ show b

-- need flexible instances
--instance Foo [Char] where
--    asFoo s = "Foo [Char]: " ++ s
