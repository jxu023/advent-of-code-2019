import Data.Char (ord, isDigit)
import Debug.Trace (trace)
import Data.Array (elems, listArray, Array, (!))
import System.IO (openFile, hGetContents, IOMode(ReadMode))

fft :: Int -> Array Int Int -> Int
fft pos arr =
    let n = length . elems $ arr
        basePattern = [0, 1, 0, -1]
        period = length basePattern * pos

        values = zip [0..] basePattern >>= \(baseIndex, coeff) ->
            let inds = filter (>= 0) . takeWhile (< n) $ [0..] >>= \seqIndex ->
                    let start = pos * baseIndex + seqIndex * period - 1
                    in [start..(start+pos-1)]
            in if coeff == 0 then []
                             else map ((coeff *) . (arr !)) inds

        ans = (`mod` 10) . abs . foldr (+) 0 $ values
    -- in trace ("fft pos " ++ show pos ++ " has " ++ show (length values) ++ " values") ans
    in ans

step :: Array Int Int -> Array Int Int
step arr =
    let n = length . elems $ arr
        phase = listArray (0, n-1) $ map (flip fft arr) [1..n]
    in trace ("n is " ++ show n ++ "\nstep " ++ (show . elems $ phase)) $ phase

stepPhases :: Int -> [Int] -> [Int]
stepPhases reps input =
    let toArray lst = listArray (0, length lst - 1) lst
    in elems . last . take (reps + 1) $ iterate step (toArray input)

msg :: [Int] -> [Int]
msg input =
    let listToNum lst = fst $ foldr (\dig (num, base)
                                        -> (dig * base + num, base * 10))
                                    (0, 1)
                                    lst
        offset = listToNum $ take 7 input
    in take 8 . drop offset . stepPhases 100 $ input

strToDigits = map (\c -> ord c - (ord '0')) . filter isDigit

solveDay16 :: String -> [Int]
solveDay16 str = msg . concat . replicate 10000 . strToDigits $ str

debugDay16 :: String -> [Int]
debugDay16 = stepPhases 9 . strToDigits

main :: IO ()
main = do
    print "hi"
    contents <- openFile "day16.input" ReadMode >>= hGetContents
    print $ solveDay16 contents
    -- print contents
    -- print $ debugDay16 contents
    print "bye"
