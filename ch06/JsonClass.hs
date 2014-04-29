{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

newtype JAry a = JAry { fromJAry :: [a] } deriving (Eq, Ord, Show)
newtype JObj a = JObj { fromJObj :: [(String, a)] } deriving (Eq, Ord, Show)

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject (JObj JValue)
            | JArray (JAry JValue)
              deriving (Eq, Ord, Show)

type JSONError = String

class JSON a where
    toJValue :: a -> JValue
    fromJValue :: JValue -> Either JSONError a

instance JSON JValue where
    toJValue = id
    fromJValue = Right

instance JSON Bool where
    toJValue = JBool
    fromJValue (JBool b) = Right b
    fromJValue _ = Left "not a JSON boolean"

instance JSON String where
    toJValue = JString
    fromJValue (JString s) = Right s
    fromJValue _ = Left "not a JSON string"

doubleToJValue :: (Double -> a) -> JValue -> Either JSONError a
doubleToJValue f (JNumber v) = Right (f v)
doubleToJValue _ _ = Left "not a JSON number"

instance JSON Int where
    toJValue = JNumber . realToFrac
    fromJValue = doubleToJValue round

instance JSON Integer where
    toJValue = JNumber . realToFrac
    fromJValue = doubleToJValue round

instance JSON Double where
    toJValue = JNumber
    fromJValue = doubleToJValue id

foldrm :: (Monad m) => (a -> b -> m b) -> b -> [a] -> m b
foldrm _ z [] = return z
foldrm f z (a:as) = foldrm f z as >>= (\z' -> f a z')

instance (JSON a) => JSON (JAry a) where
    toJValue = JArray . JAry . map toJValue . fromJAry
    fromJValue (JArray a) = (fmap JAry . foldrm step [] . fromJAry) a
        where
          step jv r = fmap (:r) (fromJValue jv)
    fromJValue _ = Left "not a jarray"

instance (JSON a) => JSON (JObj a) where
    toJValue = JObject . JObj . fmap (fmap toJValue) . fromJObj
    fromJValue (JObject o) = (fmap JObj . foldrm step [] . fromJObj) o
        where
          step (k,jv) r = fmap (\x -> (k,x):r) (fromJValue jv)
    fromJValue _ = Left "not a jobject"
               
