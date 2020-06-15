import Coord
import Control.Monad
import Data.Array
import Data.List
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

moveToDir :: Char -> Int
moveToDir c = case lookup c [('a', -1), ('s', 0), ('d', 1)] of Just x  -> x
                                                               Nothing -> error "invalid player move"

playGame :: ArcadeGame -> IO Int
playGame game = do
    print game
    move <- getChar
    let game' | elem move ['0', '1', '2'] = case coverBall game move of Just x  -> x
                                                                        Nothing -> error "can't cover!"
              | move == 'w'               = breakBlock game
              | otherwise                 = stepGame game (moveToDir move)
    if isOver game' then do print game'
                            print "Oh no! you died!, time to resurrect!"
                            playGame game
                            -- return (gameScore game')
                    else playGame game'
          
p2Solution :: String -> IO Int
p2Solution contents =
    let state     = initState (parseMem contents)
        gameStart = bootGame state
    in playGame gameStart

ballLanding :: ArcadeGame -> Int
ballLanding game =
    let loc = ballLocation . tracker $ game
    in if coordY loc == 21 then coordX loc
                           else ballLanding $ stepGame game 0

sign :: Int -> Int
sign x | x < 0     = -1
       | x > 0     = 1
       | otherwise = 0

movePaddle :: ArcadeGame -> Int -> ArcadeGame
movePaddle game offset
    | isOver game = game
    | offset == 0 = game
    | otherwise   = movePaddle (stepGame game (sign offset)) (offset - sign offset)

hitBall :: ArcadeGame -> ArcadeGame
hitBall game
    | isOver game  = game
    | height == 21 = stepGame game 0
    | otherwise    = hitBall (stepGame game 0)
    where height = coordY . ballLocation . tracker $ game

-- only takes -1, 0, or 1 as offset
catchBall :: ArcadeGame -> Int -> ArcadeGame
catchBall game offset =
    let x = ballLanding game
        paddleDst =  x + offset
        paddleSrc = coordX . paddleLocation . tracker $ game
    in  hitBall $ movePaddle game (paddleDst - paddleSrc)

coverBall :: ArcadeGame -> Char -> Maybe ArcadeGame
coverBall game option =
    let hits          = [-1, 0, 1]
        games         = map (\x -> catchBall game x) hits
        winnableGames = filter (not . isOver) games
        index         = (read :: String -> Int) [option]
    in if null winnableGames then Nothing
                             else Just $ winnableGames !! (min index (length winnableGames - 1))

countBlocks :: ArcadeGame -> Int
countBlocks = blockCount . tracker

breakBlockHelper :: [ArcadeGame] -> Int -> ArcadeGame
breakBlockHelper q count =
    let options   = ['0', '1', '2']
        peelMaybe = map (\(Just x) -> x) . filter (\x -> case x of Nothing -> False
                                                                   _       -> True
                                                  )
        q'        = peelMaybe . concat . map (\game -> [coverBall game move | move <- options]) $ q
        goal game = not (isOver game) && countBlocks game < count || countBlocks game == 0
    in case find goal q of Just game -> game
                           Nothing   -> if null q' then error "can't break anymore blocks!"
                                                   else breakBlockHelper q' count

breakBlock :: ArcadeGame -> ArcadeGame
breakBlock game = breakBlockHelper [game] (countBlocks game)

main = do
    putStrLn "Advent of Code Day 13"

    gameFile <- openFile "day13.input" ReadMode
    contents <- hGetContents gameFile

    finalScore <- p2Solution contents
    putStr $ "final score is " ++ show finalScore

    hClose gameFile
    putStr "\nend"
