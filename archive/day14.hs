import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Functor.Identity
import Debug.Trace
import Data.List
import ReactionTypes
import ReactionParser
import System.IO
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

type Set = Set.Set

type Pool = Map (Int, String) Int

data Process = Process { reactions :: Map String (Int, [Term])
                       , rank      :: Map String Int
                       , pool      :: Pool
                       } deriving (Show)

addPool :: Pool -> Pool -> Pool
addPool = Map.unionWith (+)

-- dead code
subtractPool :: Pool -> Pool -> Pool
subtractPool = Map.differenceWith (\a b -> Just $ if a - b >= 0 then a - b else 0)

-- dead code
dividePool :: Pool -> Pool -> Int
dividePool a b =
    let keys = map fst . Map.toList $ a
        get pool k = fromIntegral $ pool Map.! k
        ratios = map (\k -> ceiling (get a k / get b k)) keys
    in foldr max (head ratios) ratios

scalePool :: Int -> Pool -> Pool
scalePool x = Map.map (* x)

toPool :: [Term] -> Map String Int -> Pool
toPool terms ranks =
    let r chem         = ranks Map.! chem
        kv (chem, amt) = ((r chem, chem), amt)
    in Map.fromList $ map kv terms

getIngredient :: Map (Int, String) Int -> ((Int, String), Int)
getIngredient = head . filter ((/= "ORE") . snd . fst) . Map.toList

ceilingDivide :: Int -> Int -> Int
ceilingDivide a b = ceiling $ fromIntegral a / fromIntegral b

simplify :: Process -> Process
simplify process =
    let ((r, chem), needed) = getIngredient (pool process)
        poolCur             = Map.delete (r, chem) (pool process)
        (produced, terms)   = reactions process Map.! chem

        basePool            = toPool terms (rank process)
        poolBuyMax          = scalePool (ceilingDivide needed produced) basePool
    in process { pool = addPool poolCur poolBuyMax }

onlyOre :: Pool -> Bool
onlyOre pool = Map.size pool == 1 && (snd . fst $ Map.elemAt 0 pool) == "ORE"

consumeProcess :: Process -> Int
consumeProcess process | onlyOre (pool process) = snd $ Map.elemAt 0 (pool process)
                       | otherwise              = consumeProcess $ simplify process

reverseMap :: Map String [String] -> Map String [String]
reverseMap orig =
    let keys                    = "ORE" : (map fst $ Map.toList orig)
        nodes                   = Map.fromList $ zip keys (repeat [])
        kvs                     = Map.toList orig
        addEdges (dst, srcs) m  = foldr (add dst) m srcs
            where add dst src m = Map.adjust (dst:) src m
    in foldr addEdges nodes kvs

-- https://stackoverflow.com/questions/28549336/is-there-way-to-represent-static-data-in-haskell-or-is-there-any-other-elegant
-- need to do more reading about monad transformers aka stacks...
topoSort :: Map String [String] -> [String]
topoSort m =
    let neighbors x = Map.findWithDefault [] x m
        go :: String -> StateT (Set String) Data.Functor.Identity.Identity [String]
        go root = (root :) . concat . reverse <$>
            forM (neighbors root) (\y -> get >>= \vis ->
                if Set.member y vis
                   then return []
                   else put (Set.insert y vis) >> go y)
    in evalState (go "ORE") (Set.singleton "ORE")

initRank :: Reactions -> Map String Int
initRank reactions =
    let deps = reverseMap . Map.map (map fst . snd) $ reactions
        order = topoSort deps
        len = length order
    in Map.fromList $ zip order [len-1,len-2..0]

initProcess :: Int -> Reactions -> Process
initProcess x reactions =
    let ranks = initRank reactions
    in Process reactions ranks (toPool [("FUEL", x)] ranks)

-- some amount of fuel that needs more than 1 trillion ORE
maxFuel :: Reactions -> Int
maxFuel reactions = head . filter (> 1000000000000) . map (\x -> minOre (2^x) reactions) $ [1..]

-- use binary search to find the max amount of FUEL which uses no more than 1 trillion ORE
minOre :: Int -> Reactions -> Int
minOre x reactions = consumeProcess . initProcess x $ reactions

-- get highest value where pred is true
binarySearch :: Int -> Int -> (Int -> Int) -> (Int -> Bool) -> Int
binarySearch l r f pred | p && not p1 = m
                        | pred mv     = binarySearch m r f pred
                        | otherwise   = binarySearch l (m-1) f pred
    where m = (l + r) `div` 2
          mv = f m
          p = pred mv
          p1 = pred . f $ m + 1

main :: IO()
main = do
    reactions <- openFile "day14.input" ReadMode
                    >>= hGetContents
                    >>= return . parseReactions
    putStrLn "maxFuel"
    print $ maxFuel reactions
    putStrLn "maxFuel for 1 trillion ore"
    print $ binarySearch 0 (maxFuel reactions) (flip minOre reactions) (<= 1000000000000)
