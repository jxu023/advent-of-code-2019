
fuelForMass:: Int -> Int
fuelForMass x = (div x 3) - 2

fuelIncluding :: Int -> Int
fuelIncluding mass
    | fuel > 0 = fuel + fuelIncluding fuel
    | otherwise  = 0
        where fuel = fuelForMass mass

totalFuel :: [Int] -> Int
totalFuel = sum . (map fuelIncluding)

main = getContents >>= \contents ->
    print . totalFuel . map (read :: String -> Int) $ lines contents
