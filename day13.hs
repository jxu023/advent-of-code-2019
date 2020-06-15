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

data Tracker = Tracker { blockCount     :: Int
                       , paddleLocation :: Coord
                       , ballLocation   :: Coord
                       }

data ArcadeGame = ArcadeGame { gameScreen :: Screen
                             , gameScore  :: Int
                             , gameState  :: IntcodeState
                             , isOver     :: Bool
                             , tracker    :: Tracker
                             }

instance Show ArcadeGame where 
    show (ArcadeGame screen score _ _ _)
        = showScreen screen ++ showScore score

showScore x = "\nScore: " ++ show x
showScreen screen =
    let (_, Coord maxX maxY) = bounds screen
    in [0..maxY] >>= \r -> ("\n" ++) $
           [0..maxX] >>= \c -> "_" ++ show (screen ! Coord c r)

countBlockTiles :: Screen -> Int
countBlockTiles = foldr countBlock 0
    where countBlock tile total = total + (if tile == Block then 1 else 0)

findTile :: Screen -> Tile -> Coord
findTile screen tile = foldr checkTile (Coord 0 0) (assocs screen)
    where checkTile (coord', t) coord = if tile == t then coord' else coord

-- boots game to first instance of RequestInput
bootGame :: IntcodeState -> ArcadeGame
bootGame state =
    let freePlay              = setMem 0 2 state
        (coords, bootedState) = collectScreenOutput freePlay
        (scores, tiles)       = span (\(c, _) -> c == Coord (-1) 0) coords
        score                 = snd $ last scores
        screen                = initScreen tiles

        blockCount            = countBlockTiles screen
        paddleLocation        = findTile screen Paddle
        ballLocation          = findTile screen Ball
        tracker               = Tracker blockCount paddleLocation ballLocation
    in ArcadeGame screen score bootedState False tracker

playerMove :: IO Int
playerMove = do
    c <- getChar
    case lookup c [('a', -1), ('s', 0), ('d', 1)] of Just x -> return x
                                                     _      -> playerMove

updateTracker :: Tracker -> (Coord, Tile, Tile) -> Tracker
updateTracker (Tracker blockCount paddleLocation ballLocation) (location, prevTile, newTile) =
    let blockCount'     = blockCount - (if prevTile == Block && newTile == Empty then 1 else 0)
        paddleLocation' = if newTile == Paddle then location else paddleLocation
        ballLocation'   = if newTile == Ball then location else ballLocation
    in Tracker blockCount' paddleLocation' ballLocation'

updateGame :: ArcadeGame -> IntcodeState -> ArcadeGame
updateGame game@(ArcadeGame screen score state over tracker) outState =
    let ((coord, val), state') = gameOutput outState
        updateScore            = coord == Coord (-1) 0
        screen'                = if updateScore then screen
                                                else screen // [(coord, intToTile val)]
        score'                 = if updateScore then val
                                                else score
        tracker'               = if updateScore then tracker
                                                else updateTracker tracker (coord, screen ! coord, tile)
        tile                   = intToTile val
        win                    = blockCount tracker' == 0
        lose                   = not updateScore && tile == Ball && coordY coord == 22
        over'                  = over || win || lose

    in ArcadeGame screen' score' state' over' tracker'

getAllOutput :: ArcadeGame -> ArcadeGame
getAllOutput game =
    let (trap, state') = runToTrap (gameState game)
    in case trap of DisplayOutput -> getAllOutput (updateGame game state')
                    RequestInput  -> game { gameState = state' }
                    CpuTerminated -> error "cpu has terminated!"

stepGame :: ArcadeGame -> Int -> ArcadeGame
stepGame game inputVal =
    let state' = stepIntcode $ (gameState game) { input = inputVal }
    in getAllOutput (game { gameState = state' })

playGame :: ArcadeGame -> IO Int
playGame game = do
    print game
    move <- playerMove
    let game' = stepGame game move
    if isOver game' then do print game'
                            return (gameScore game')
                    else playGame game'
          
p2Solution :: String -> IO Int
p2Solution contents =
    let state     = initState (parseMem contents)
        gameStart = bootGame state
    in playGame gameStart

ballLanding :: ArcadeGame -> Int
ballLanding game =
    let ballLoc = undefined
    in undefined

movePaddle :: ArcadeGame -> Int -> ArcadeGame
movePaddle = undefined

catchBall :: ArcadeGame -> Int -> ArcadeGame
catchBall game offset =
    let x = ballLanding game
        paddleDst = x + offset
        paddleSrc = coordX . paddleLocation . tracker $ game
    in  movePaddle game (paddleDst - paddleSrc)

main = do
    putStrLn "Advent of Code Day 13"

    gameFile <- openFile "day13.input" ReadMode
    contents <- hGetContents gameFile

    finalScore <- p2Solution contents
    putStr $ "final score is " ++ show finalScore

    hClose gameFile
    putStr "\nend"
