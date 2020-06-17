import Debug.Trace
import ReactionTypes
import ReactionParser
import System.IO
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Merge

timesAmount :: Int -> Term -> Term
timesAmount x (chemical, amount) = (chemical, x * amount)

expand :: Reactions -> Term -> Pool
expand reactions (chemical, needed) =
    let (produced, inputs) = reactions Map.! chemical
        rounds             = ceiling (fromIntegral needed / fromIntegral produced)
    in Map.fromList $ map (timesAmount $ trace (show rounds ++ " of " ++ chemical) rounds) inputs

combinePools :: Pool -> Pool -> Pool
combinePools = Merge.merge Merge.preserveMissing Merge.preserveMissing (Merge.zipWithMatched (\k u v -> u + v))

sortChemicals :: Reactions -> [String]
sortChemicals reactions =
    let ins :: Map String Int
        ins = Map.foldr countFormula zeroCount reactions
        zeroCount = Map.fromSet (const 0) (Data.Set.fromList .
    in

simplifyTerm :: Pool -> Reactions -> Pool
simplifyTerm pool reactions =
    let term@(chemical, _) = head . filter ((/= "ORE") . fst) . Map.toList $ pool
        pool'              = Map.delete chemical pool
        poolB              = expand reactions term
    in combinePools pool' poolB

minOre :: Reactions -> Int
minOre reactions = go (expand reactions ("FUEL", 1))
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
