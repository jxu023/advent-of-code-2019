module Queue where

import Control.Monad.Trans.State.Lazy

data Queue a = Queue { pushStack :: [a]
                     , popStack :: [a]
                     } deriving (Show)

push :: a -> State (Queue a) ()
push x = state f
    where f (Queue on off) = ((), Queue (x:on) off)

pop :: State (Queue a) a
pop = state f
    where f (Queue [] []) = error "cannot pop on empty Queue"
          f (Queue ons []) = f $ Queue [] (reverse ons)
          f (Queue ons (x:xs)) = (x, Queue ons xs)

testQueue :: State (Queue Int) Int
testQueue = do
    push 5
    push 6
    push 4
    _ <- pop
    b <- pop
    return b

emptyQ :: Queue Int
emptyQ = Queue [] []

testQ = runState testQueue emptyQ
