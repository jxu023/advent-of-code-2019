import Data.List (sortBy)
import Lib (splitOn)

parseWire :: String -> [String]
parseWire = splitOn ','

opPt f (sx, sy) (dx, dy) = (f sx dx, f sy dy)
multPt a b = opPt (*) a (b, b)
addPt a b = opPt (+) a b

fillLine :: Point -> Point -> Point -> [Point]
fillLine start@(sx, sy) dir end
    | start == end = [end]
    | otherwise    = (sx, sy):(fillLine (addPt start dir) dir end)

type Point = (Int, Int)
fillWire :: Point -> [String] -> [Point]
fillWire _ [] = []
fillWire src ((c:num):wire) = pts ++ fillWire endpt wire
    where dir = case c of 'U' -> (0, 1)
                          'D' -> (0, (-1) * 1)
                          'L' -> ((-1) * 1, 0)
                          'R' -> (1, 0)
          dist = (read :: String -> Int) num
          pts = fillLine (addPt src dir) dir endpt
          endpt = addPt src (multPt dir dist)

pointValue (a, b) = abs a + abs b
minPointVal :: [Point] -> Int
minPointVal points = foldr (min . pointValue) (maxBound :: Int) points

findFirstEqual :: [[Point]] -> Maybe Point
findFirstEqual wires
    | any ((==) 0 . length) wires = Nothing
    | all ((==) val . head) wires = Just (head $ head wires)
    | otherwise                   = findFirstEqual (map update wires)
        where update wire
                  | pointValue (head wire) == minVal = tail wire
                  | otherwise                  = wire
              minVal = minPointVal $ map head wires
              val = head $ head wires

cmpPts a b 
    | a == b                                                   = EQ
    | ptsA < ptsB
        || ptsA == ptsB && fst a < fst b
            || ptsA == ptsB && fst a == fst b && snd a < snd b = LT
    | otherwise                                                = GT
    where ptsA = pointValue a
          ptsB = pointValue b
sortPts pts = sortBy cmpPts pts

main = getContents >>=
    print . findFirstEqual . map (sortPts . fillWire (0, 0) . parseWire) . lines
