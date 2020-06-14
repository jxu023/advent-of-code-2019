import Coord
import Control.Monad
import Data.Array
import Debug.Trace
import Intcode
import System.IO

data Tile = Empty | Wall | Block | Paddle | Ball | NotATile
          deriving (Eq)

instance Show Tile where
    show Empty  = " "
    show Wall   = "|"
    show Block  = "#"
    show Paddle = "P"
    show Ball   = "O"

intToTile 0 = Empty
intToTile 1 = Wall
intToTile 2 = Block
intToTile 3 = Paddle
intToTile 4 = Ball
intToTile _ = NotATile

gameOutput :: IntcodeState -> ((Coord, Int), IntcodeState)
gameOutput stateX = let xVal           = output stateX
                        (_, stateY)    = runToTrap stateX
                        yVal           = output stateY
                        (_, nextState) = runToTrap stateY
                        tileVal        = output nextState
                    in ((Coord xVal yVal, tileVal), nextState)

collectScreenOutput :: IntcodeState -> ([(Coord, Int)], IntcodeState)
collectScreenOutput coldState =
    let go tiles state | trap == DisplayOutput = go (tile:tiles) nextState
                       | otherwise             = (tiles, trapState)
            where (trap, trapState) = runToTrap state
                  (tile, nextState) = gameOutput trapState
    in go [] coldState

type Screen = Array Coord Tile

initScreen :: [(Coord, Int)] -> Screen
initScreen tiles =
    let emptyArr                      = listArray (Coord 0 0, Coord maxX maxY) (repeat Empty)
        (maxX, maxY)                  = foldr maxAx (0, 0) tiles
        maxAx (Coord x y, _) (mx, my) = (max mx x, max my y)
    in emptyArr // reverse (map (\(k, v) -> (k, intToTile v)) tiles)

data ArcadeGame = ArcadeGame { gameScreen :: Screen
                             , gameScore  :: Int
                             , gameState  :: IntcodeState
                             , blockCount :: Int
                             , lastMove   :: (Coord, Tile)
                             , isOver     :: Bool
                             }

instance Show ArcadeGame where 
    show (ArcadeGame screen score _ _ _ _)
        = showScreen screen ++ showScore score

showScore x = "\nScore: " ++ show x
showScreen screen =
    let (_, Coord maxX maxY) = bounds screen
    in [0..maxY] >>= \r -> ("\n" ++) $
           [0..maxX] >>= \c -> "_" ++ show (screen ! Coord c r)

countBlockTiles :: Screen -> Int
countBlockTiles = foldr countBlock 0
    where countBlock tile total = total + (if tile == Block then 1 else 0)

-- boots game to first instance of RequestInput
bootGame :: IntcodeState -> ArcadeGame
bootGame state =
    let freePlay              = setMem 0 2 state
        (coords, bootedState) = collectScreenOutput freePlay
        (scores, tiles)       = span (\(c, _) -> c == Coord (-1) 0) coords
        score                 = snd $ last scores
        screen                = initScreen tiles
        blockCount            = countBlockTiles screen
    in ArcadeGame screen score bootedState blockCount (Coord 0 0, Empty) False

playerMove :: IO Int
playerMove = do
    c <- getChar
    case lookup c [('a', -1), ('s', 0), ('d', 1)] of Just x -> return x
                                                     _      -> playerMove

playGame :: ArcadeGame -> IO Int
playGame game = do
    print game
    move <- playerMove
    let game' = stepGame game move
    if isOver game' then return (gameScore game')
                    else playGame game'

stepGame :: ArcadeGame -> Int -> ArcadeGame
stepGame game inputVal =
    let (trap, state') = runToTrap ((gameState game) {input = inputVal})
    in case trap of RequestInput  -> game { gameState = state' }
                    DisplayOutput -> updateGame game state'
                    GameOver      -> game { gameState = state'}

updateGame :: ArcadeGame -> IntcodeState -> ArcadeGame
updateGame game@(ArcadeGame screen score state blockCount lastMove over) outState =
    let ((coord, val), state') = gameOutput outState
        updateScore            = coord == Coord (-1) 0
        screen'                = if updateScore then screen
                                                else screen // [(coord, intToTile val)]
        score'                 = if updateScore then val
                                                else score
        tile                   = intToTile val
        blockCount'            = blockCount - (if screen ! coord == Block && screen' ! coord == Empty then 1 else 0)
        lastMove'              = if updateScore then lastMove else (coord, tile)
        win                    = blockCount' == 0
        lose                   = tile == Paddle && coordY coord <= 22
        over'                  = win || lose
    in ArcadeGame screen' score' state' blockCount' lastMove' over'
          
p2Solution :: String -> IO Int
p2Solution contents =
    let state     = initState (parseMem contents)
        gameStart = bootGame state
    in playGame gameStart

main = do
    putStrLn "Advent of Code Day 13"

    gameFile <- openFile "day13.input" ReadMode
    contents <- hGetContents gameFile

    finalScore <- p2Solution contents
    putStr $ "final score is " ++ show finalScore

    hClose gameFile
    putStr "\nend"
