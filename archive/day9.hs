import Lib

main = do
    print "Advent of Code: Day 9"
    print "Relative base parameter and instruction"
    print "allow for access to memory beyond initial load of instruction"

    contents <- getContents

    let state = initState (parseMem contents) [2]
    print "initial state"
    print state

    print "final state"
    print $ runIntcode state
