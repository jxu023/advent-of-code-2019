module ReactionParser
    ( parseReactions
    ) where

import qualified Data.Map.Strict as Map
import ReactionTypes

type Map = Map.Map

parseTerms :: [String] -> [Term]
parseTerms []                = []
parseTerms (num:chemical:xs) =
    (filter (/= ',') chemical, read num :: Int):(parseTerms xs)

parseOutput :: [String] -> Term
parseOutput (a:b:[]) = (b, read a :: Int)

splitTwo :: String -> [String] -> ([String], [String])
splitTwo divider = go []
    where go left (x:xs) | x == divider = (reverse left, xs)
                         | otherwise    = go (x:left) xs

parseReaction :: String -> (Term, [Term])
parseReaction str =
    let (ins, outs) = splitTwo "=>" (words str)
    in (parseOutput outs, parseTerms ins)

reactionListToMap :: [(Term, [Term])] -> Reactions
reactionListToMap = Map.fromList . map keyVal
    where keyVal ((chemical, quantity), inputs) =
              (chemical, (quantity, inputs))

parseReactions :: String -> Reactions
parseReactions = reactionListToMap . map parseReaction . lines
