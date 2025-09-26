-- 06. Tuplas
intP :: (Int, Int)
intP = (2,3)

menorMaior:: Int -> Int -> Int -> (Int, Int)
menorMaior a b c = (maior, menor)
  where maior | (a >= b) && (a >= c) = a
              | (b >= a) && (b >= c) = b
              | otherwise = c
        menor | (a <= b) && (a <= c) = a
              | (b <= a) && (b <= c) = b
              | otherwise = c
