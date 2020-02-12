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

ctrlToDir 'a' = (-1)
ctrlToDir 's' = 0
ctrlToDir 'd' = 1

inBounds (Coord x y) (Coord ax ay, Coord bx by) = x >= ax && x <= bx && y >= ay && y <= by
outBounds a b = not $ inBounds a b

updateGame :: ArcadeGame -> IntcodeState -> ((Coord, Int), ArcadeGame)
updateGame game@(ArcadeGame screen score state) outState
    | hasOutput outState = let (diff@(coord, val), state') = gameOutput outState
                               updateScore = coord == Coord (-1) 0
                               screen' | updateScore = screen
                                       | otherwise   = screen // [(coord, intToTile val)]
                               score' | updateScore = val
                                      | otherwise   = score
                               game' = ArcadeGame screen' score' state'
                           in (diff, game')
    | otherwise = error "updateGame should be called on state w/ output"

printGame :: (Coord, Int) -> ArcadeGame -> ArcadeGame -> IO ()
printGame (coord, val) prev cur | emptyingTile && (prevTile == Paddle || prevTile == Ball) = print prev
                                | otherwise                                                = print cur
    where emptyingTile = intToTile val == Empty
          prevTile = gameScreen prev ! coord

updateBackups :: [ArcadeGame] -> ((Coord, Int), ArcadeGame) -> (Bool, [ArcadeGame])
updateBackups backups ((coord, val), cur) | needBackup = (True, drop 1 backups)
                                          | saveBackup = (False, cur:backups)
                                          | otherwise  = (False, backups)
    where isBall = intToTile val == Ball
          needBackup = isBall && coordY coord == 22
          saveBackup = isBall && coordY coord == 21

playGame :: ArcadeGame -> [ArcadeGame] -> IO Int
playGame game@(ArcadeGame screen score state) backups
    | terminatedIntcode state = return score
    | hasOutput state         = do let (diff, game') = updateGame game state
                                   printGame diff game game'

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
        where play s = runStdinToOutput ctrlToDir s

botPlay :: Int -> ArcadeGame -> ArcadeGame
botPlay expire game | expire == 0 = game
                    | otherwise   = botPlay (expire-1) . waitBall . saveBall game' . loseBall $ g'
    where game' = game { gameState = runToInterrupt (gameState game) }
          g' = trace ("botplay\n" ++ show game') $ game'

loseBall :: ArcadeGame -> Int
loseBall g = let s' = runWithInput [0] (gameState g)
                 ((Coord x y, val), g') = updateGame g s'
             in if y == 21 && intToTile val == Ball then x
                                                    else trace ("loseball " ++ show g') $ loseBall g'

getBallY game = foldr gb 0 $ assocs (gameScreen game)
    where gb (Coord x y, v) orig = if v == Ball then y else orig

-- moves paddle to given location
saveBall :: ArcadeGame -> Int -> ArcadeGame
saveBall game loc 
    = let ploc = foldr getPaddle 0 $ assocs (gameScreen game)
          getPaddle (Coord x y, v) orig = if v == Paddle then x else orig
          game' = waitHit game
      in movePaddle ploc loc game'

waitHit :: ArcadeGame -> ArcadeGame
waitHit game = let game' = runToNextGame $ game
                   state' = runWithInput [0] (gameState game')
                   (_, game'') = updateGame game' state'
               in runToNextGame game''

movePaddle :: Int -> Int -> ArcadeGame -> ArcadeGame
movePaddle ploc loc game = let dir | ploc < loc  = 1
                                   | ploc == loc = 0
                                   | otherwise   = (-1)
                               state' = runWithInput [dir] (gameState game)
                               (_, game') = updateGame game state'
                               game'' = runToNextGame game'
                           in if dir == 0 then game
                                          else trace ("moving paddle " ++ show game') $ movePaddle (ploc + dir) loc game''

runToNextGame :: ArcadeGame -> ArcadeGame
runToNextGame g = let s = gameState g
                  in g { gameState = runToInterrupt s }

waitBall :: ArcadeGame -> ArcadeGame
waitBall g = let s' = runWithInput [0] (gameState g)
                 ((Coord x y, val), g') = updateGame g s'
             in if getBallY g == 21 then g else if y == 21 && intToTile val == Ball then runToNextGame g'
                                                                                    else trace ("waitball" ++ show g') waitBall g'

p2Solution :: String -> IO Int
p2Solution contents
    = let state = initState (parseMem contents) []
          gameStart = bootGame state
      in do print gameStart
            let game = botPlay 5 gameStart
            putStrLn $ show game ++ "\nback to the human!"
            playGame game []

main = do
    putStrLn "Advent of Code Day 13"

    gameFile <- openFile "day13.input" ReadMode
    contents <- hGetContents gameFile

    print . p1Solution $ contents
    p2Solution $ contents

    hClose gameFile
    putStr "\nend"
