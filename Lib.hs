module Lib where

-- misc --
splitOn :: Char -> String-> [String]
splitOn delim raw = go [] raw delim
    where go tok (r:raw) d
              | r == d    = [tok] ++ go [] raw d
              | otherwise = go (tok ++ [r]) raw d
          go tok [] d = [tok]

gcdHelper a b | b == 0 || rem == 0  = b
              | otherwise           = greatestCommonDivisor rem b
    where rem = mod a b

greatestCommonDivisor a b | a < b     = greatestCommonDivisor b a
                          | otherwise = gcdHelper a b

leastCommonMultiple :: Int -> Int -> Int
leastCommonMultiple a b = (a * b) `div` greatestCommonDivisor a b

-- Comparing between list elems --
extractElem cmp results = foldr cmp (head results) (tail results)

-- permute --
oneRemoved :: [a] -> [(a, [a])]
oneRemoved lst = go [] (head lst) (tail lst)
    where go left cur [] = [(cur, left)]
          go left cur right = (cur, left ++ right):others
              where others = (go (cur:left) (head right) (tail right))

permute :: [a] -> [[a]]
permute [] = [[]]
permute bag = oneRemoved bag >>= \(one, others) -> map (one:) (permute others)
