import Data.List (find)

-- uses a char as delimiter to split a string
splitOn :: Char -> String-> [String]
splitOn delim raw = go [] raw delim
    where go tok (r:raw) d
              | r == d    = [tok] ++ go [] raw d
              | otherwise = go (tok ++ [r]) raw d
          go tok [] d = [tok]

type IntcodeMem = [(Int, Int)]

getVal :: Int -> IntcodeMem -> Int
getVal pos mem = case result of Nothing  -> 0
                                Just val -> snd val
    where result = find ((==) pos . fst) mem

setVal :: (Int, Int) -> IntcodeMem -> IntcodeMem
setVal (k, v) [] = [(k, v)]
setVal (k, v) ((key, val):mem)
    | k == key  = (k, v):mem
    | otherwise = (key, val):(setVal (k, v) mem)

setVals :: IntcodeMem -> IntcodeMem -> IntcodeMem
setVals [] mem = mem
setVals (v:vals) mem = setVals vals (setVal v mem)

deref ptr mem = getVal (getVal (ptr) mem) mem

opIntcode f pc mem = setVal (dst, result) mem
    where result = f src1 src2
          src1 = deref (pc + 1) mem
          src2 = deref (pc + 2) mem
          dst  = getVal (pc + 3) mem

addOpcode pc mem = opIntcode (+) pc mem
multOpcode pc mem = opIntcode (*) pc mem

-- Takes program counter and Intcode program.
-- Returns final memory state when program halts
runIntcode :: Int -> IntcodeMem -> IntcodeMem
runIntcode _ [] = []
runIntcode pc mem
    = case getVal pc mem of 1  -> runIntcode (pc + 4) (addOpcode pc mem)
                            2  -> runIntcode (pc + 4) (multOpcode pc mem)
                            99 -> mem
                            _  -> (-1, -1):mem

parseIntcode :: String -> [(Int, Int)]
parseIntcode = zip [0..] . map (read :: String -> Int) . splitOn ','



-- given result in pos 0 and an Intcode program
-- find the (noun, verb) that produces the result
findNounVerb :: Int -> IntcodeMem -> (Int, Int)
findNounVerb val mem = case find equalVal runs of Nothing          -> (-1, -1)
                                                  Just (mem, pair) -> pair
    where equalVal run = (getVal 0 (fst run)) == val
          runs = enumerateNounVerbRuns mem

tryNounVerb mem noun verb = runIntcode 0 $ setVals [(1, noun), (2, verb)] mem

enumerateNounVerbRuns :: IntcodeMem -> [(IntcodeMem, (Int, Int))]
enumerateNounVerbRuns mem = map process nvs
    where pos = 5
          nvs = [(noun, verb) | noun <- [0..pos], verb <- [0..pos]]
          process (n, v) = (tryNounVerb mem n v, (n, v))

main = getContents >>=
    print . map (\(mem, pair) -> (snd $ head mem, pair)) . enumerateNounVerbRuns . parseIntcode
    -- print . (\(n, v) -> "noun " ++ show n ++ " verb " ++ show v) . findNounVerb 19690720 . parseIntcode

-- scanned through #s for a pattern

-- res = 1090665 + 300000 * noun + 1 * verb
-- solve for res == 19690720 to minimize noun + verb .. or something like that
--
-- verb = rem (19690720 - 1090665) 300000 = 55
-- noun = div (19690720 - 1090665) 300000 = 62
--
