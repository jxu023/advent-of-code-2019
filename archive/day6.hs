import Lib (splitOn)
import Debug.Trace
import qualified Data.Map.Strict as Map
type Map = Map.Map

type Orbits = Map String [String]

update :: [(String, String)] -> Orbits
update = foldr f Map.empty
    where f (k, v) m = if Map.member k m then Map.adjust (v:) k m
                                     else Map.insert k [v] m

parseOrbits :: String -> Orbits
parseOrbits = update . map ((\(a:b:[]) -> (a, b)) . splitOn ')') . lines

totalOrbits :: Orbits -> Int
totalOrbits orbits = 1 + go "COM" 0
    where go :: String -> Int -> Int
          go planet indirs = case Map.lookup planet orbits of Nothing    -> indirs - 1
                                                              Just moons -> orbited moons
                                where orbited moons = sum ((indirs - 1):(length moons):(totals moons))
                                      totals moons = [go moon (indirs + 1) | moon <- moons] 

data Loc = Loc { reachable :: Bool, dist::Int }
         deriving (Show)

instance Eq Loc where
    (==) a b = reachable a == reachable b && dist a == dist b

instance Ord Loc where
    compare a b | reachable a && not (reachable b) || reachable a && reachable b && dist a < dist b = LT
                | a == b                                                                            = EQ
                | otherwise                                                                         = GT

addLoc a b | reachable a && reachable b = Loc True (dist a + dist b)
           | otherwise                  = Loc False (dist a)

distToHelper cur dest orbits =
    let planetAndMoonMaps moons = (curMap, map snd moonMaps)
            where moonMaps = [(moon, distToHelper moon dest orbits) | moon <- moons]
                  curMap = Map.singleton cur closestLoc
                  closestLoc = if reachable minLoc then minLoc { dist = dist minLoc + 1 }
                                                   else minLoc
                  minLoc = foldr min (head locs) (tail locs)
                  locs = map (\(moon, moonMap) -> moonMap Map.! moon) moonMaps
    in case Map.lookup cur orbits of Nothing    -> Map.singleton cur (Loc (cur == dest) 0)
                                     Just moons -> uncurry (foldr Map.union) (planetAndMoonMaps moons)

distTo :: String -> Orbits -> Map String Loc
distTo dest orbits = distToHelper "COM" dest orbits

transfersToSanta orbits =
    let distToSanta = distTo "SAN" orbits
        distToYou = distTo "YOU" orbits
        distYouToSanta = Map.unionWith addLoc (trace (show distToSanta) distToSanta) distToYou
    in  Map.foldr min (distYouToSanta Map.! "COM") (trace (show distYouToSanta) distYouToSanta)

main = do
    contents <- getContents
    print . totalOrbits . parseOrbits $ contents

    print . transfersToSanta . parseOrbits $ contents

-- find the lowest common ancestor and dist from common ancestor to YOU and SANTA
-- propagate min dist to SANTA,YOU up from SANTA and YOU nodes

-- dist from YOU planet to SANTA planet = (transfers from dist from YOU to santa) - 2
