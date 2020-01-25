{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics (Generic)
import Lib (extractElem)
import Data.Hashable
import qualified Data.HashSet as HashSet
import Debug.Trace
import Data.List (sort)
import qualified Queue as Queue

type HashSet = HashSet.HashSet
type Queue = Queue.Queue

data Coord = Coord { coordX :: Int, coordY :: Int }
           deriving (Generic, Hashable)

instance Show Coord where
    show (Coord x y) = "(" ++ show x ++ ", " ++ show y ++ ")"

instance Eq Coord where
    (==) a b = coordX a == coordX b && coordY a == coordY b

angle (Coord x y) = let result = atan2 fx fy
                        fx = fromIntegral x
                        fy = fromIntegral y
                    in result + (if result < 0 then 2 * pi else 0)

instance Ord Coord where
    compare a b | angle a < angle b || angle a == angle b && dist a < dist b = LT
                | a == b                                                     = EQ
                | otherwise                                                  = GT
        where dist (Coord x y) = sqrt . fromIntegral $ x * x + y * y

gcdHelper a b | b == 0 || rem == 0  = b
              | otherwise           = greatestCommonDivisor rem b
    where rem = mod a b

greatestCommonDivisor a b | a < b     = greatestCommonDivisor b a
                          | otherwise = gcdHelper a b

sign num | num < 0   = (-1)
         | otherwise = 1

simplify dx dy = Coord (unit y x) (unit x y)
    where gcd = greatestCommonDivisor (abs dx) (abs dy)
          gcd' = if gcd == 0 then 1 else gcd
          x = div dx gcd'
          y = div dy gcd'
          unit a b = if a == 0 then sign b else b

slope p@(Coord px py) q@(Coord qx qy)
    = simplify (qx - px) (qy - py)

addCoord c1 c2 = Coord (coordX c1 + coordX c2) (coordY c1 + coordY c2)

pointsBetween p1 p2 =
    let dp = slope p1 p2
        go a b | a == b    = []
               | otherwise = a:(go (addCoord a dp) b)
    in go (addCoord p1 dp) p2

parseAsteroids contents =
    let rows = lines contents
        asteroids = zip [0..] rows >>= \(r, row) ->
            zip [0..] row >>= \(c, elem) ->
                if elem == '#' then [Coord c r]
                               else []
    in asteroids

asteroidsSeenFrom :: Coord -> HashSet Coord -> Int
asteroidsSeenFrom p1 asteroids = HashSet.foldr f 0 asteroids
    where f p2 sum = let blocked = any (\p -> HashSet.member p asteroids) (pointsBetween p1 p2)
                     in -- trace ("tr " ++ show p1 ++ " " ++ show p2) $
                        if p1 == p2 || blocked then sum else sum + 1

asteroidsSeen :: [Coord] -> [(Int, Coord)]
asteroidsSeen coords = zip scores coords
    where scores = map (\c -> asteroidsSeenFrom c coordSet)
                       coords
          coordSet = HashSet.fromList coords

cmpFst v1 v2 = if fst v1 > fst v2
                  then v1
                  else v2

-- best view is asteroid that can see the most other asteroids
asteroidWithBestView :: [Coord] -> (Int, Coord)
asteroidWithBestView = extractElem cmpFst . asteroidsSeen

oppCoord (Coord x y) = Coord (-x) (-y)

resetOrigin :: Coord -> [Coord] -> [Coord]
resetOrigin c = map (addCoord (oppCoord c))

blastAsteroids :: Queue Coord -> [Coord]
blastAsteroids coordQueue = go (angle $ Coord 1 1) coordQueue 0
    where go prev q cnt | Queue.empty q                  = []
                        -- | trace (show cnt ++ " " ++ show q) False = undefined
                        | cnt == 0 || angle elem /= prev = elem : go (angle elem) q' cnt'
                        | otherwise                      = go prev (Queue.push elem q') cnt'
            where (elem, q') = Queue.pop q
                  cnt' = if cnt > 0 then cnt - 1 else Queue.length q'

mirrorY (Coord x y) = Coord x (-y)

laserBlastOrder :: Coord -> [Coord] -> [Coord]
laserBlastOrder laser
    = map mirrorY
    . resetOrigin (oppCoord (mirrorY laser))
    . blastAsteroids
    . Queue.fromList
    . tail
    . sort
    . resetOrigin (mirrorY laser)
    . map mirrorY
-- sort asteroids by (angle, distance)
-- put sorted into a queue
-- iterate over queue, order in which asteroids popped and not put back is final answer

main = do
    print "Advent of Code: Day 10"
    print "asteroid monitoring station"

    contents <- getContents
    let coords = parseAsteroids contents
        maxLocation = asteroidWithBestView coords
    print maxLocation

    let laser = snd maxLocation
        laseredOrder = laserBlastOrder laser coords
    print $ laseredOrder !! 199
