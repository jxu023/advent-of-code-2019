module Queue where

data Queue a = Queue { pushStack :: [a], popStack :: [a] }

fromList lst = Queue [] lst

instance Show a => Show (Queue a) where
    show q = "Queue: " ++ (show (popStack q ++ reverse (pushStack q)))

empty (Queue pushStack popStack) = null pushStack && null popStack

push x q = q { pushStack = x:(pushStack q) }

top q@(Queue pushStack popStack) | null popStack = top (Queue [] (reverse pushStack))
                                 | otherwise     = (head popStack, q)

pop q = (\(x, q') -> (x, q' { popStack = tail (popStack q') }))
        (top q)

poppush q = let (elem, q') = pop q
            in push elem q'

setTop x q = tq { popStack = x:(tail (popStack tq)) }
    where tq = snd $ top q

length (Queue pushStack popStack) = Prelude.length pushStack + Prelude.length popStack
