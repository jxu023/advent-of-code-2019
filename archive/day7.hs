import Data.Array
import Lib
import Debug.Trace

type PhaseSetting = [Int]

initAmplifiers :: Mem -> PhaseSetting -> Queue IntcodeState
initAmplifiers mem = foldr (\x q -> push (initAmp x) q) (Queue [] []) . reverse
    where initAmp x = IntcodeState 0 mem [x] []

runAmplifiers :: Queue IntcodeState -> Int
runAmplifiers amps | opcode == 99 && emptyQ popped = head . input $ cur
                   | opcode == 99                  = runAmplifiers chainPopped
                   | opcode /= 4                   = runAmplifiers (setTop runCur amps)
                   | otherwise                     = runAmplifiers chained
    where opcode = mod ((mem cur) ! (pc cur)) 100
          (cur, popped) = pop amps
          runCur = stepIntcode cur
          signal state = head . output $ state

          next = fst $ top popped
          chained = setTop (next { input = (input next) ++ [signal runCur] }) (push runCur popped)
          chainPopped = setTop (next { input = input cur } ) popped

addInput0 q = setTop top' q
    where top' = x { input = input x ++ [0] }
          (x, tq) = top q

testFeedbackSetting setting mem = runAmplifiers (addInput0 $ initAmplifiers mem setting)

testSetting :: PhaseSetting -> Mem -> Int
testSetting setting mem = foldr outSignal 0 setting
    where outSignal x input = head . output $ runIntcode (IntcodeState 0 mem [x, input] [])

getMaximal results = foldr (\a b -> if fst a > fst b then a else b)
                           (head results)
                           (tail results)

permuteRange :: Int -> [[Int]]
permuteRange len = permute [0..(len - 1)]

allFeedbackSignals mem = [(testFeedbackSetting x mem, x) | x <- permute [5..9]]
allSignals mem = [(testSetting ps mem, ps) | ps <- permuteRange 5]

maximalSetting :: Mem -> (Int, PhaseSetting)
maximalSetting mem = getMaximal $ allSignals mem
maximalFeedbackSetting mem = getMaximal $ allFeedbackSignals mem

printMaxSeq :: IO ()
printMaxSeq = do
    contents <- getContents
    let mem = parseMem contents

    print . maximalSetting $ mem

main = do
    print "hi"

    contents <- getContents
    let mem = parseMem $ contents
    -- print . maximalSetting $ mem

    -- print . testFeedbackSetting [9,8,7,5,6] $ mem
    -- print . allFeedbackSignals $ mem
    print . maximalFeedbackSetting $ mem
