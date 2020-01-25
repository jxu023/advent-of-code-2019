import Coord
import Control.Monad
import Data.Array
import Debug.Trace
import Intcode
import System.IO

data Tile = Empty | Wall | Block | Paddle | Ball | NotATile
          deriving (Eq)

instance Show Tile where
    show Empty = " "
    show Wall = "|"
    show Block = "#"
    show Paddle = "P"
    show Ball = "O"

intToTile 0 = Empty
intToTile 1 = Wall
intToTile 2 = Block
intToTile 3 = Paddle
intToTile 4 = Ball
intToTile _ = NotATile

gameOutput :: IntcodeState -> ((Coord, Int), IntcodeState)
gameOutput stateX = let xVal = getOutput stateX
                        (yVal, stateY)    = runToOutput stateX
                        (tileVal, stateT) = runToOutput stateY
                    in ((Coord xVal yVal, tileVal), stateT)

runArcadeGame :: IntcodeState -> ([(Coord, Int)], IntcodeState)
runArcadeGame coldState
    = let go tiles state | hasOutput stateX = go (tile:tiles) stateT
                         | otherwise        = (tiles, stateX)
            where stateX = runToInterrupt state
                  (tile, stateT) = gameOutput stateX
      in go [] coldState

type Screen = Array Coord Tile

initScreen :: [(Coord, Int)] -> Screen
initScreen tiles
    = let emptyArr = listArray (Coord 0 0, Coord maxX maxY) $ repeat Empty
          (maxX, maxY) = foldr maxAx (0, 0) tiles
          maxAx (Coord x y, _) (mx, my) = (max mx x, max my y)
      in emptyArr // reverse (map (\(k, v) -> (k, intToTile v)) tiles)

countBlockTiles :: Screen -> Int
countBlockTiles = foldr countBlock 0
    where countBlock tile total = total + case tile of Block -> 1
                                                       _     -> 0

p1Solution :: String -> Int
p1Solution contents
    = let state = initState (parseMem contents) []
          screen = initScreen . fst $ runArcadeGame state
      in countBlockTiles screen

-- part two --

data ArcadeGame = ArcadeGame { gameScreen :: Screen
                             , gameScore :: Int
                             , gameState :: IntcodeState
                             }

instance Show ArcadeGame where 
    show (ArcadeGame screen score _)
        = showScreen screen ++ showScore score

showScore x = "\nScore: " ++ show x
showScreen screen
    = let (_, Coord maxX maxY) = bounds screen
      in [0..maxY] >>= \r -> ("\n" ++) $
            [0..maxX] >>= \c -> "_" ++ show (screen ! Coord c r)

bootGame :: IntcodeState -> ArcadeGame
bootGame state = let freePlay = setMem 0 2 state
                     (coords, bootedState) = runArcadeGame freePlay
                     (scores, tiles) = span (\(c, _) -> c == Coord (-1) 0) coords
                     score = snd $ last scores
                     screen = initScreen tiles
                 in ArcadeGame screen score bootedState

movePaddle 'a' = (-1)
movePaddle 's' = 0
movePaddle 'd' = 1

inBounds (Coord x y) (Coord ax ay, Coord bx by) = x >= ax && x <= bx && y >= ay && y <= by
outBounds a b = not $ inBounds a b

updateAndPrintGame :: ArcadeGame -> IntcodeState -> IO ((Coord, Int), ArcadeGame)
updateAndPrintGame game@(ArcadeGame screen score state) outState
    | hasOutput state = let (diff@(coord, val), state') = gameOutput outState
                            updateScore = coord == Coord (-1) 0
                            screen' | updateScore = screen
                                    | otherwise   = screen // [(coord, intToTile val)]
                            score' | updateScore = val
                                   | otherwise   = score
                            game' = ArcadeGame screen' score' state'
                        in do printGame diff game game'
                              return (diff, game')
    | otherwise = error "updateGame should be called on state w/ output"

printGame :: (Coord, Int) -> ArcadeGame -> ArcadeGame -> IO ()
printGame (coord, val) prev cur | emptyingTile && (prevTile == Paddle || prevTile == Ball) = print prev
                                | otherwise                                                = print cur
    where emptyingTile = intToTile val == Empty
          prevTile = gameScreen prev ! coord

updateBackups :: [ArcadeGame] -> ((Coord, Int), ArcadeGame) -> (Bool, [ArcadeGame])
updateBackups backups ((coord, val), cur) | needBackup = (True, drop 2 backups)
                                          | saveBackup = (False, cur:backups)
                                          | otherwise  = (False, backups)
    where isBall = intToTile val == Ball
          needBackup = isBall && coordY coord == 23
          saveBackup = isBall && coordY coord == 21

-- TODO use this in updateBackups and loseBall
lostBall :: (Coord, Int) -> Bool
lostBall = undefined

playGame :: ArcadeGame -> [ArcadeGame] -> IO Int
playGame game@(ArcadeGame screen score state) backups
    | terminatedIntcode state = return score
    | hasOutput state         = do (diff, game') <- updateAndPrintGame game state
                                   let (lostBall, backups') = updateBackups backups (diff, game')
                                       loadSave = playGame (backups !! 1) backups'
                                       continueGame = do state' <- play (gameState game')
                                                         playGame game' { gameState = state' } backups'
                                   if lostBall then loadSave
                                               else continueGame
    | otherwise               = do state' <- play state
                                   let game' = game { gameState = state' }
                                   print game'
                                   playGame game' backups
        where play s = runStdinToOutput movePaddle s

-- assumes game is at winnable state
botPlay :: Int -> ArcadeGame -> ArcadeGame
botPlay expire game | expire == 0 = game
                    | otherwise   = botPlay (expire-1) . saveBall game . loseBall $ game

-- does nothing and waits for ball to go below paddle
-- returns X location of where paddle to block ball
loseBall :: ArcadeGame -> Int
loseBall game@(ArcadeGame screen _ state)
    = undefined

-- moves paddle to given location
saveBall :: ArcadeGame -> Int -> ArcadeGame
saveBall = undefined

p2Solution :: String -> IO Int
p2Solution contents
    = let state = initState (parseMem contents) []
          gameStart = bootGame state
      in do print gameStart
            playGame gameStart []

-- bootGame state = let freePlay = setMem 0 2 state
--                      (coords, bootedState) = runArcadeGame freePlay

main = do
    putStrLn "Advent of Code Day 13"

    gameFile <- openFile "day13.input" ReadMode
    contents <- hGetContents gameFile

    print . p1Solution $ contents
    p2Solution $ contents

    hClose gameFile
    putStr "\nend"
