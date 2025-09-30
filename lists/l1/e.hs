main :: IO ()
main = do
    s <- getLine
    print (sumTo (read s))

sumTo:: Int -> Int
sumTo 0 = 0
sumTo n = n + sumTo(n-1)