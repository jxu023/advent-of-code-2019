import Data.List (sortBy)
import Lib (splitOn)
import qualified Data.Set as Set
type Set = Set.Set


parseWire :: String -> [String]
parseWire = reverse . splitOn ','

data Point = Point { getx :: Int, gety :: Int, steps :: Int }

pointValue p = abs (getx p) + abs (gety p)

instance Eq Point where
    (==) a b = getx a == getx b && gety a == gety b

instance Ord Point where
    compare a b 
        | ptsA < ptsB || (ptsA == ptsB && (getx a < getx b || (getx a == getx b && gety a < gety b))) = LT
        | a == b      = EQ
        | otherwise   = GT
        where ptsA = pointValue a
              ptsB = pointValue b

instance Show Point where
    show (Point x y s) = "(" ++ show x ++ ", " ++ show y ++ ") steps " ++ show s

opPt f (Point x1 y1 s) (Point x2 y2 _) = Point (f x1 x2) (f y1 y2) (s + 1)
addPt a b = opPt (+) a b
multPt a b = opPt (*) a (Point b b 0)

fillLine :: Point -> Point -> Point -> Set Point
fillLine start dir end
    | start == end = Set.singleton start
    | otherwise    = Set.insert start (fillLine (addPt start dir) dir end)

fillWire :: [String] -> Set Point
fillWire lines = snd $ foldr go (Point 0 0 0, Set.empty) lines
    where go (c:num) (src, pts) = (endpt { steps = steps src + dist }, Set.union line pts)
              where endpt = addPt src (multPt dir dist)
                    line = fillLine (addPt src dir) dir endpt
                    dir = case c of 'U' -> Point 0 1 0
                                    'D' -> Point 0 (-1) 0
                                    'L' -> Point (-1) 0 0
                                    'R' -> Point 1 0 0
                    dist = (read :: String -> Int) num

getMax :: Ord a => [a] -> a
getMax (x:xs) = foldr max x xs

findFirstEqual :: [Set Point] -> Maybe Point
findFirstEqual (w:ws)
    | Set.size common == 0 = Nothing
    | otherwise            = Just $ Set.elemAt 0 common
    where common = foldr Set.intersection w ws

result (Just p) = pointValue p
result _ = -1

stepCmp a b
    | steps a < steps b = a
    | otherwise         = b

minStepLst :: [Point] -> Point
minStepLst pts = foldr stepCmp (Point 0 0 (maxBound :: Int)) pts

minStepSet :: Set Point -> Point
minStepSet = minStepLst . Set.toList

pointStep :: Point -> Set Point -> Int
pointStep pt pts = steps $ Set.elemAt (Set.findIndex pt pts) pts

wireSteps :: [Set Point] -> [Point] -> [Point]
wireSteps wires pts = pts >>= \pt -> return $ pt { steps = sumSteps pt }
    where sumSteps pt = foldr (+) 0 (wires >>= \wire -> return $ pointStep pt wire)

main = do
    contents <- getContents
    -- print contents
    let wires = map (fillWire . parseWire) $ lines contents
    -- print wires
    let common = (\(w:ws) -> foldr Set.intersection w ws) wires

    let wire_steps = wireSteps wires (Set.toList common)

    let msp = minStepLst wire_steps

    print $ msp

    -- print $ Set.elemAt 0 common

    -- let pos0 = findFirstEqual wires
    -- print pos0
    -- print $ result pos0
