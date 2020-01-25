import Lib (extractElem)

parseLayers :: Int -> String -> [String]
parseLayers cnt = dropEmpty . snd . foldr f (0, [[]])
    where f = (\c (len, (str:strs)) -> if (len == cnt) then (1, [c]:str:strs)
                                                       else (len + 1, (c:str):strs))
          dropEmpty lst = if null (head lst) then tail lst else lst

count :: Eq a => a -> [a] -> Int
count targ = foldr (\c cnt -> if c == targ then cnt + 1 else cnt) 0

zipKey :: Eq a => (a -> b) -> [a] -> [(b, a)]
zipKey f = map (\x -> (f x, x))

minKey p1 p2 = if fst p1 < fst p2 then p1 else p2

layerWithLeast :: Char -> [String] -> String
layerWithLeast c = snd . extractElem minKey . zipKey (count c)

imageCheckSum :: String -> Int
imageCheckSum = (\layer -> count '1' layer * count '2' layer)
                . layerWithLeast '0' . parseLayers (25 * 6)

visible a b = if b /= '2' then b else a

decodeImage :: Int -> [String] -> String
decodeImage size = foldr f (take size $ repeat '2') . reverse
    where f layer img = zipWith visible layer img

showImage :: Int -> String -> String
showImage row = unlines . parseLayers row . map conv
    where conv c | c == '0' = ' '
                 | c == '1' = '#'
                 | otherwise = '!'

main = do
    print "Advent of Code: Day 8"
    contents <- getContents
    let image = filter (/= '\n') contents
    print image
    print $ length image
    print . imageCheckSum $ image
    let row = 25
    let imgSize = row * 6
    putStr . showImage row . decodeImage imgSize . parseLayers imgSize $ image
