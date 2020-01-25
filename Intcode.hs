module Intcode where

import Debug.Trace
import qualified Data.IntMap.Strict as IntMap
import Lib

type IntMap = IntMap.IntMap

type Mem = IntMap Int
data IntcodeState = IntcodeState { pc :: Int
                                 , mem :: Mem
                                 , input :: [Int]
                                 , output :: [Int]
                                 , base :: Int
                                 }
instance Show IntcodeState where
    show state@(IntcodeState pc mem input output base)
        = "state: " ++ (show pc ++ " " ++ show input ++ " " ++ show (reverse output) ++ " " ++ show base)
          ++ "\nopcode: " ++ show (getOp state)

parseMem :: String -> Mem
parseMem str = let mem = map (read :: String -> Int) . splitOn ',' $ str
                      in IntMap.fromAscList $ zip [0..] mem 

initState :: Mem -> [Int] -> IntcodeState
initState mem input = IntcodeState 0 mem input [] 0

getOp state = mod (get (mem state) (pc state)) 100

get mem pc | IntMap.member pc mem = mem IntMap.! pc
           | otherwise            = 0

stepIntcode :: IntcodeState -> IntcodeState
stepIntcode state@(IntcodeState pc mem input output base)
    = -- trace (show pc ++ ": "
      --       ++ show instr ++ " " ++ show (param 1) ++ " " ++ show (param 2) ++ " " ++ show (setloc 3)) $
      case op of 1 -> aluOp (+)
                 2 -> aluOp (*)
                 3 -> state { pc = pc + 2
                            , mem = IntMap.insert (setloc 1) (head input) mem
                            , input = tail input
                            }
                 4 -> state { pc = pc + 2
                            , output = ((param 1):output)
                            }
                 5 -> state { pc = jmp (/= 0) }
                 6 -> state { pc = jmp (== 0) }
                 7 -> state { pc = pc + 4,
                              mem = store (<) }
                 8 -> state { pc = pc + 4
                            , mem = store (==)
                            }
                 9 -> state { pc = pc + 2
                            , base = base + param 1
                            }
                 _ -> error $ "unexpected op code" ++ show op
    where param x = (paramMode x) (val x)
          val x = get mem (pc + x)
          setloc x = case modeIndicator x of 0 -> val x
                                             2 -> val x + base
                                             _ -> error "unexpected mode indicator for setting"
          paramMode x = case modeIndicator x of 0 -> (get mem)
                                                1 -> id
                                                2 -> (get mem) . (+ base)
                                                _ -> error $ "unexpected mode indicator " ++ show (modeIndicator x)
          modeIndicator x = div (mod instr (10 ^ y)) (10 ^ (y - 1))
              where y = x + 2

          aluOp f = state { pc = pc + 4
                          , mem = IntMap.insert (setloc 3) (f (param 1) (param 2)) mem }
          op = mod instr 100
          instr = get mem pc

          jmp f = if f (param 1) then param 2 else pc + 3
          store f = if f (param 1) (param 2) then smem 1 else smem 0
              where smem x = IntMap.insert (setloc 3) x mem

setMem :: Int -> Int -> IntcodeState -> IntcodeState
setMem k v state = state { mem = IntMap.insert k v (mem state) }

runIntcode :: IntcodeState -> IntcodeState
runIntcode state@(IntcodeState pc mem _ _ _) =
    case val of 99 -> state
                _  -> runIntcode (stepIntcode state)
    where val = mod (get mem pc) 100

terminatedIntcode state = getOp state == 99
needsInput        state = getOp state == 3
hasOutput         state = (not $ needsInput state) && (not $ terminatedIntcode state)

type InputVal = Int
runWithInput :: [InputVal] -> IntcodeState -> IntcodeState
runWithInput inVal state
    = let run = runToInterrupt
          state' = run state
      in if needsInput state' then run $ stepIntcode state { input = inVal }
                              else state'

type OutputVal = Int
runToGetOutput :: [InputVal] -> IntcodeState -> (OutputVal, IntcodeState)
runToGetOutput inv state = (outv, state')
    where outv = head $ output state'
          state' = runWithInput inv state

getOutput :: IntcodeState -> Int
getOutput state = head $ output state

runToInterrupt :: IntcodeState -> IntcodeState
runToInterrupt state
    = case getOp state of 3  -> state
                          4  -> state'
                          99 -> state
                          _  -> runToInterrupt state'
    where state' = stepIntcode state

runToOutput state = let state' = runToInterrupt state
                    in (getOutput state', state')

runStdinToOutput :: (Char -> Int) -> IntcodeState -> IO IntcodeState
runStdinToOutput inFun state
    = let state' = runToInterrupt state
          ctrl s | needsInput s        = do inVal <- getChar
                                            let s' = stepIntcode s { input = [inFun inVal] }
                                            runStdinToOutput inFun s'
                 | otherwise           = return s
      in ctrl state'
