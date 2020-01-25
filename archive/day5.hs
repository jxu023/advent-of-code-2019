import Lib

main = do 
    contents <- getContents
    
    print . length . runIntcode 0 . parseIntcodeMem $ contents
