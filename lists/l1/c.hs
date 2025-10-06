main = do
    a <- getLine
    let result = minMaxCartao a
    print result


minMaxCartao :: String -> (Double, Double)
minMaxCartao s = (minimum valores, maximum valores)
  where valores = listNumbers (wordsWhen (==';') s)

listNumbers:: [String] -> [Double]
listNumbers [] = []
listNumbers (a:as) | isDouble a = stringToDouble a:listNumbers as
                   | otherwise = listNumbers as 

stringToDouble :: String -> Double
stringToDouble s = read s :: Double

isDouble:: String -> Bool
isDouble [] = False
isDouble (a:as) | a == '.' = True
                | otherwise = isDouble as

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'



