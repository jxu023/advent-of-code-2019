{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import Data.Hashable
import qualified Data.HashSet as HashSet
import GHC.Generics (Generic)
import Lib (oneRemoved, leastCommonMultiple)

type HashSet = HashSet.HashSet

data MoonAxis = MoonAxis { velocity :: Int
                         , position :: Int
                         } deriving ( Eq
                                    , Generic
                                    , Hashable
                                    , Show
                                    )

type Moon = [MoonAxis]

showMoon :: [MoonAxis] -> String
showMoon (xa:ya:za:[]) = getS position "pos" ++ ", " ++ getS velocity "vel"
    where getS get label = label ++ "=<x= " ++ show (get xa)
                                 ++ ", y= " ++ show (get ya)
                                 ++ ", z= " ++ show (get za) ++ ">"

showMoons :: [Moon] -> String
showMoons = unlines . map showMoon

parseMoons :: String -> [Moon]
parseMoons = map (initMoon . parseMoonPos) . lines

parseMoonPos :: String -> [Int]
parseMoonPos = parsePositions . words
    where parsePositions (sx:sy:sz:[])
              = map (read . init) [drop 3 sx ,drop 2 sy, drop 2 sz]

initMoon :: [Int] -> Moon
initMoon ps = ps >>= \pos -> return (MoonAxis 0 pos)

orderingToDiff LT = 1
orderingToDiff EQ = 0
orderingToDiff GT = (-1)

applyGravity :: [MoonAxis] -> [MoonAxis]
applyGravity moons
    = oneRemoved moons >>= \(m, ms) -> return $
        let (MoonAxis v p) = m
            dvs = map position ms >>= \q -> return $
                orderingToDiff (compare p q)
            dv = sum dvs
        in m { velocity = v + dv }

applyVelocity :: [MoonAxis] -> [MoonAxis]
applyVelocity moons = map f moons
    where f (MoonAxis v p) = MoonAxis v (v + p)

stepAxs = applyVelocity . applyGravity

-- array might be a better fit for moon type..
transpose :: [[a]] -> [[a]]
transpose mat = foldr f cols mat
    where cols = replicate (length (head mat)) []
          f row cols = zipWith (:) row cols

stepMoons :: [Moon] -> [Moon]
stepMoons moons =
    let axs = transpose moons
        axs' = axs >>= return . stepAxs
    in transpose axs'

simulate :: Int -> [Moon] -> [Moon]
simulate x ms = last . take (x + 1) $ iterate stepMoons ms

moonEnergy :: Moon -> Int
moonEnergy axs = sabs velocity * sabs position
    where sabs get = sum . map (abs . get) $ axs

energyAfterSimul :: Int -> [Moon] -> Int
energyAfterSimul steps = sum . map moonEnergy . simulate steps

solution :: [Moon] -> Int
solution = energyAfterSimul 1000

printMs = putStr . ('\n':) . showMoons

moonPeriod :: [MoonAxis] -> Int
moonPeriod axs = go axs HashSet.empty
    where go cur visited | HashSet.member cur visited = 0
                         | otherwise                  = 1 + go (stepAxs cur)
                                                               (HashSet.insert cur visited)

moonsRepeated :: [Moon] -> Int
moonsRepeated moons
    = let axsl = transpose moons
          periods :: [Int]
          periods = axsl >>= return . moonPeriod
      in foldr leastCommonMultiple 1 periods

main = do
    print "Advent of Code: Day 12"
    print "N-body problem"
    contents <- getContents
    let moons = parseMoons contents
    printMs moons

    -- printMs $ simulate 10 moons
    -- print $ energyAfterSimul 10 moons
    -- print $ solution moons

    print $ moonsRepeated moons

    print "end"
