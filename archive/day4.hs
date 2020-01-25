import Lib (splitOn)

parseRange :: String -> [Int]
parseRange range = case splitOn '-' range of (a:b:[]) -> [(rd a)..(rd b)]
                                             _ -> []
    where rd = read :: String -> Int

hasSixDigits num = num >= 111111 && num <= 999999

onesDigit num = mod num 10
tensDigit num = div (mod num 100) 10
hundredsDigit num = div (mod num 1000) 100

foldrDigits pred conn acc num
    | num == 0  = acc
    | otherwise = conn (pred (onesDigit num) (tensDigit num))
                       (foldrDigits pred conn acc (div num 10))

anyEqualAdjacent = foldrDigits (==) (||) False
allNonDecreasing = foldrDigits (>=) (&&) True

-- oops! that's not part of the right criteria!
maxRepeatedAdjDigits num = go 1 0 0 num
    where go maxRep rep dig num
              | num == 0             = maxRep
              | dig == onesDigit num = go (max maxRep nrep) nrep dig nnum
              | otherwise            = go maxRep 1 (onesDigit num) nnum
            where nrep = rep + 1
                  nnum = div num 10

runs num = go 0 0 num
    where go dig run num
              | num == 0  = [run]
              | dig == od = go dig (run + 1) nnum
              | otherwise = run:(go od 1 nnum)
            where nnum = div num 10
                  od = onesDigit num

hasRunLen2 num = any (== 2) (runs num)

valid num = all ($ num) [hasSixDigits, hasRunLen2, allNonDecreasing]

main = getContents >>=
    print . length . filter valid . parseRange

