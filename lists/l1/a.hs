main = do
    a <- getLine
    b <- getLine
    c <- getChar
    let result = isReplica a (read b) c
    print result

isReplica :: String -> Int -> Char -> Bool
isReplica str int char | length str /= int = False
                       | otherwise = length [x | x <-str, x==char] == int
