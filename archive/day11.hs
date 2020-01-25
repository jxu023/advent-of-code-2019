-- {-# OPTIONS_GHC -Wall #-}

import Coord
import Debug.Trace
import qualified Data.HashMap.Strict as HashMap
import Data.List (elemIndices)
import Intcode

type HashMap = HashMap.HashMap

data Panel = Black | White
                deriving (Eq, Show)
panelToInt Black = 0
panelToInt White = 1
panelToChar Black = '.'
panelToChar White = '#'

intToPanel x = case x of 0 -> Black
                         1 -> White
                         _ -> error $ "cannot convert " ++ show x ++ " to a panel"

-- panel is Black if not in paintedPanels
data Hull = Hull { paintedPanels :: HashMap Coord Panel
                 , painterLoc :: Coord
                 , painterState :: IntcodeState
                 , painterDir :: Coord
                 } deriving (Show)

-- input will be fed in runPainter
initPainter mem = initState mem []

initWhitePanel = HashMap.singleton (Coord 0 0) White

initHull state = Hull initWhitePanel (Coord 0 0) state (Coord 0 1)

getPanel :: Coord -> HashMap Coord Panel -> Panel
getPanel coord panels = maybe Black id (HashMap.lookup coord panels)

runPainter :: Hull -> Hull
runPainter hull@(Hull panels loc state dir)
    = let panel = panelToInt $ getPanel loc panels
          state' = runToOutput [panel] state
          hull' = hull { painterState = state' }
          step = runPainter . turnAndMovePainter . paintPanel
      in -- trace ("painter is at " ++ show loc ++ " w/ " ++ show panel ++ " dir " ++ show dir) $
         if terminatedIntcode state' then hull'
                                     else step hull'

paintPanel :: Hull -> Hull
paintPanel hull@(Hull panels loc state _)
    = let panel = intToPanel . head . output $ state
          panels' = HashMap.insert loc panel panels
          state' = runToOutput [] state
      in hull { paintedPanels = panels', painterState = state' }

dirs = Coord 0 1 : Coord 1 0 : Coord 0 (-1) : Coord (-1) 0 : dirs
turnDir shift dir = (dirs !! ) . shift . (!! 1) . elemIndices dir $ dirs
turnLeft = turnDir (+ (-1))
turnRight = turnDir (+ 1)

intToTurn 0 = turnLeft
intToTurn 1 = turnRight
intToTurn x = undefined

turnAndMovePainter hull@(Hull _ loc state dir)
    = let dir' = (intToTurn . head . output $ state) dir
          loc' = addCoord dir' loc
      in hull { painterLoc = loc', painterDir = dir' }

boundCoord panels choose extract = HashMap.foldrWithKey f 0 panels
    where f k _ x = choose (extract k) x

coordRange panels = let f = boundCoord panels
                    in ((f min coordX, f max coordX), (f min coordY, f max coordY))

showPanels ((minX, maxX), (minY, maxY)) panels
    = [maxY, (maxY - 1)..minY] >>= \y -> ("\n" ++) $
          [minX..maxX] >>= \x -> 
              [panelToChar $ getPanel (Coord x y) panels, ' ']

main = do
    print "Advent of Code: Day 11"
    print "Hull painting robot"
    contents <- getContents
    let memory = parseMem contents
        hull = initHull . initPainter $ memory
        panels = paintedPanels $ runPainter hull
        range = coordRange panels
    print range
    putStrLn . showPanels range $ panels
    print "end"

