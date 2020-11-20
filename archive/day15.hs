import Intcode
import Coord

import Control.Monad.State
import Debug.Trace

import qualified Data.Map as Map
import qualified Data.Set as Set

type Map = Map.Map
type Set = Set.Set

data Cell = Droid | Wall | Empty

instance Show Cell where
    show Droid = "D"
    show Wall = "#"
    show Empty = " "

newtype Area = Area (Map Coord Cell)

instance Show Area where
    show (Area cells) =
        let coords = map fst $ Map.toList cells
            compareDim cmp accessor =
                foldr (\coord accum -> let x = accessor coord
                                       in if x `cmp` accum then x else accum)
                      ((\coord -> accessor coord) $ head coords)
                      coords
            minX = compareDim (<) coordX
            maxX = compareDim (>) coordX
            minY = compareDim (<) coordY
            maxY = compareDim (>) coordY
            toAreaCoord (Coord x y) = Coord (x + minX) (maxY - y)
            get screenCoord = Map.findWithDefault Empty (toAreaCoord screenCoord) cells
            showScreenCoord = show . get
            showRow y = [0..(maxX - minX)] >>= \x -> showScreenCoord (Coord x y)
            showScreen = [0..(maxY - minY)] >>= \y -> showRow y ++ "\n"
        in showScreen

runToOutput :: IntcodeState -> Int -> (Int, IntcodeState)
runToOutput state input =
    let state' = stepIntcode $ state { input = input }
        stateWithOutput = snd $ runToTrap state'
        out = output stateWithOutput
        nextState = snd $ runToTrap stateWithOutput
    in (out, nextState)

addLoc :: Coord -> Int -> (Coord, Int)
addLoc (Coord x y) dir = (\c -> (c, dir)) $
    case dir of 1 -> Coord x (y - 1)
                2 -> Coord x (y + 1)
                3 -> Coord (x - 1) y
                4 -> Coord (x + 1) y
                _ -> error ("invalid direction " ++ show dir)

-- bfs :: IntcodeState -> Int -> [(Coord, [Int])] -> [(Coord, [Int])] -> Set Coord -> Int
bfs state dist [] [] visited = error "nothing left, didn't find oxygen system"
bfs state dist [] nextq visited = bfs state (dist + 1) nextq [] visited
bfs state dist q nextq visited =
    let (coord, path) = head q
        curState = foldr (\dir state -> let (out, state') = runToOutput state dir
                                        in if out == 1 then state' else error ("what!? " ++ show out))
                         state
                         (path)
        dirs = [1, 2, 3, 4]
        results = map (fst . runToOutput curState) dirs
        foundOxygenSystem = elem 2 results
        
        emptydirs = map fst . filter ((== 1) . snd) $ zip dirs results
        goaldir = fst . addLoc coord . head . map fst . filter ((== 2) . snd) $ zip dirs results

        coords = filter (not . flip Set.member visited . fst) . map (addLoc coord) $ emptydirs
        neighs = map (\(coord, x) -> (coord, x:path)) coords
        visited' = foldr (\coord vis -> Set.insert coord vis) visited (map fst neighs)
    in if foundOxygenSystem then (visited', dist + 1, goaldir)
                            else bfs state dist (tail q) (neighs ++ nextq) visited'

distToOxygenSystem state = bfs state 0 [(Coord 0 0, [])] [] (Set.singleton (Coord 0 0))

expand :: Coord -> [Coord]
expand (Coord x y) = [Coord (x+1) y, Coord (x-1) y, Coord x (y-1), Coord x (y+1)]

floodfill [] [] empties time = time
floodfill [] nextq empties time = trace ("floodfill nextq is " ++ show nextq)
                                        $ floodfill nextq [] empties (time + 1)
floodfill q nextq empties time =
    let coord = head q
        neighs = filter (flip Set.member empties) $ expand coord
        empties' = foldr Set.delete empties neighs
    in if null empties' then time
                        else floodfill (tail q) (neighs ++ nextq) empties' time

timeToFloodOxygen :: Coord -> Set Coord -> Int
timeToFloodOxygen coord empties = floodfill [] [coord] empties 0

main :: IO ()
main = do
    initialState <- parseIntcodeFile "day15.input"
    let (visited, dist, oxygenSystemCoord) = distToOxygenSystem (snd $ runToTrap initialState)
    putStrLn $ "dist is " ++ show dist
    putStrLn $ "oxygen starts flowing from " ++ show oxygenSystemCoord
    print $ timeToFloodOxygen oxygenSystemCoord visited
