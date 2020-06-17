module ReactionTypes where

import qualified Data.Map.Strict as Map

type Map = Map.Map

type Term = (String, Int)
type Reactions = Map String (Int, [Term])
type Pool = Map String Int
