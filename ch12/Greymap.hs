module Greymap (Greymap(..)) where

import qualified Data.ByteString.Lazy as L
import Text.Printf (printf)

data Greymap = Greymap {
      greyWidth :: Int,
      greyHeight :: Int,
      greyMax :: Int,
      greyData :: L.ByteString
    } deriving (Eq)

instance Show Greymap where
    show (Greymap w h m _) = printf "Greymap %dx%d %d" w h m
