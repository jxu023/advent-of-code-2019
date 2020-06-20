import Debug.Trace
import ReactionTypes
import ReactionParser
import System.IO
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

type Set = Set.Set

data CFreq = CFreq { term :: Term
                   , freq :: Int
                   }
instance Ord CFreq where
    a `compare` b = freq a `compare` freq b

type Pool = Set CFreq

toCFreq :: Map String Int -> Term -> Int
toCFreq freqMap term = CFreq Term (freqMap Map.! fst term)

countFreqs :: Reactions -> Map String Int
countFreqs reactions =
    let incrementFreqs (_, terms) freqs   = foldr incrementTerm freqs terms
        incrementTerm (chemical, _) freqs = Map.adjust (+ 1) chemical freqs
        zeroFreqs                         = Map.fromList [(chemical, 0) | chemical <- map fst $ Map.toList reactions]
    in foldr incrementFreqs zeroFreqs reactions

timesTerm :: Int -> Term -> Term
timesTerm x (chemical, amount) = (chemical, x * amount)

expand :: Map String Int -> Reactions -> Term -> Pool
expand freqs reactions (chemical, needed) =
    let (produced, inputs) = reactions Map.! chemical
        rounds             = ceiling (fromIntegral needed / fromIntegral produced)
        terms              = map (timesTerm rounds) inputs
        pool               = Set.fromList $ map (toCFreq freqs) terms
    in pool

simplifyTerm :: Pool -> Reactions -> Pool
simplifyTerm pool reactions =
    let term  = Set.elemAt 0 pool
        pool' = Set.deleteAt 0 pool
        poolB = expand freqs reactions term
    in combinePools pool' poolB

minOre :: Reactions -> Int
minOre reactions =
    let freqs = countFreqs reactions
        go :: Pool -> Int
        go pool | Set.size pool == 1 = Set.elemAt 0 pool
    in go (expand freqs reactions ("FUEL", 1))

minOre reactions = go
    where go :: Pool -> Int
          go pool | Map.size pool == 1 = pool Map.! "ORE"
                  | otherwise          = go $ simplifyTerm pool reactions

main :: IO()
main = do
    reactions <- openFile "day14.input" ReadMode
                    >>= hGetContents
                    >>= return . parseReactions
    print reactions
    putStrLn $ "need a minimum of " ++ show (minOre reactions) ++ " ORE"
