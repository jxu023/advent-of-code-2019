module Intcode where

import Debug.Trace
import qualified Data.IntMap.Strict as IntMap
import Lib

type IntMap = IntMap.IntMap

type Mem = IntMap Int
data IntcodeState = IntcodeState { pc :: Int
                                 , mem :: Mem
                                 , input :: Int
                                 , output :: Int
                                 , base :: Int
                                 }
instance Show IntcodeState where
    show state@(IntcodeState pc mem input output base)
        = "state: " ++ (show pc ++ " " ++ show input ++ " " ++ show output ++ " " ++ show base)
          ++ "\nopcode: " ++ show (getOp state)

parseMem :: String -> Mem
parseMem str = let mem = map (read :: String -> Int) . splitOn ',' $ str
               in IntMap.fromAscList $ zip [0..] mem

initState :: Mem -> IntcodeState
initState mem = IntcodeState 0 mem 0 0 0

getOp :: IntcodeState -> Int
getOp state = mod (getWord state (pc state)) 100

getWord state pc | IntMap.member pc ram = ram IntMap.! pc
                 | otherwise            = 0
  where ram = mem state

setMem :: Int -> Int -> IntcodeState -> IntcodeState
setMem k v state = state { mem = IntMap.insert k v (mem state) }

stepIntcode :: IntcodeState -> IntcodeState
stepIntcode state@(IntcodeState pc mem input output base)
    = case op of 1 -> aluOp (+)
                 2 -> aluOp (*)
                 3 -> state { pc = pc + 2
                            , mem = IntMap.insert (setloc 1) input mem
                            }
                 4 -> state { pc = pc + 2
                            , output = param 1
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
    where param x               = (paramMode x) (val x)
          val x                 = getWord state (pc + x)
          setloc x              = case modeIndicator x of 0 -> val x
                                                          2 -> val x + base
                                                          _ -> error "unexpected mode indicator for setting"
          paramMode x           = case modeIndicator x of 0 -> (getWord state)
                                                          1 -> id
                                                          2 -> (getWord state) . (+ base)
                                                          _ -> error $ "unexpected mode indicator " ++ show (modeIndicator x)
          modeIndicator x       = div (mod instr (10 ^ y)) (10 ^ (y - 1))
              where y           = x + 2

          aluOp f               = state { pc = pc + 4
                          , mem = IntMap.insert (setloc 3) (f (param 1) (param 2)) mem }
          op                    = mod instr 100
          instr                 = getWord state pc

          jmp f                 = if f (param 1) then param 2 else pc + 3
          store f               = if f (param 1) (param 2) then smem 1 else smem 0
              where smem x      = IntMap.insert (setloc 3) x mem

data Trap = RequestInput | DisplayOutput | CpuTerminated deriving (Eq)
runToTrap :: IntcodeState -> (Trap, IntcodeState)
runToTrap state
    = case getOp state of 3  -> (RequestInput, state)
                          4  -> (DisplayOutput, state')
                          99 -> (CpuTerminated, state)
                          _  -> runToTrap state'
    where state' = stepIntcode state
