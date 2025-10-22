-- 1. insere um valor numa lista ordenada
insert:: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (a:as) | x <= a = x:a:as
                | otherwise = a: insert x as

-- 2. verifica se n é primo 
ehPrimo:: Int -> Bool
ehPrimo n | n < 2 = False
          | otherwise = primoAux n (n-1)

primoAux :: Int -> Int -> Bool
primoAux n 1 = True
primoAux n m | n `mod` m == 0 = False
             | otherwise = primoAux n (m-1)

-- 3. somar o quadrado dos numeros primos num intervalo a b
-- usando filter map e fold
sumPrimeSquares1:: Int -> Int -> Int
sumPrimeSquares1 a b = foldl (+) 0 (map (\x->x*x) (filter ehPrimo [a..b]))

-- 4. use uma função lamba na questão anterior
-- feito

-- 5. reescreva a função 3 sem map filter e fold (use list compherension)
sumPrimeSquares2:: Int -> Int -> Int
sumPrimeSquares2 a b = somar [x*x| x <- [a..b], ehPrimo x]
  where somar (a:as) | as == [] = a
                     | otherwise = a + somar as