main = do
    a <- getLine
    b <- getLine
    let result = logMes a b
    print result

logMes :: String -> String -> Double
logMes target fatura = sum (getValuesOfMonth target (groupByThree (wordsWhen (==';') fatura)))

getValuesOfMonth:: String -> [[String]] -> [Double]
getValuesOfMonth _ [] = []
getValuesOfMonth target (a:as) | checkSubString target (head a) = stringToDouble (last a):getValuesOfMonth target as
                               | otherwise = getValuesOfMonth target as


stringToDouble :: String -> Double
stringToDouble s = read s :: Double

checkSubString:: String -> String -> Bool
checkSubString _ [] = False
checkSubString target str | str == target = True
                          | otherwise = checkSubString target (tail str)

groupByThree :: [a] -> [[a]]
groupByThree [] = []
groupByThree xs = take 3 xs : groupByThree (drop 3 xs)

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'