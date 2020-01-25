{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Coord where

import GHC.Generics (Generic)
import Data.Hashable
import Data.Ix

data Coord = Coord { coordX :: Int, coordY :: Int }
           deriving (Generic, Hashable, Ord, Ix)

instance Show Coord where
    show (Coord x y) = "(" ++ show x ++ ", " ++ show y ++ ")"

instance Eq Coord where
    (==) a b = coordX a == coordX b && coordY a == coordY b

addCoord c1 c2 = Coord (coordX c1 + coordX c2) (coordY c1 + coordY c2)
