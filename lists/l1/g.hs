import Data.Char (digitToInt)
main = do
    s <- getLine
    let result = btoi s
    print result

btoi:: String -> Int
btoi [] = 0
btoi str | head str == '1' = potencia2 (length str-1) + btoi (tail str)
         | otherwise = btoi (tail str)

potencia2:: Int -> Int
potencia2 0 = 1
potencia2 n = 2*potencia2 (n-1)