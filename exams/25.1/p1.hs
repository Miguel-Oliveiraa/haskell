-- 1. Escreva uma funcao que retorna toda sequencia
-- de fibonacci

-- minha solução
next:: [Int] -> [Int]
next (a:b:rest) = (a + b) : next (b:rest)

fibonacci:: [Int]
fibonacci = 0 : 1 : next fibonacci

-- solução otima
genFib:: Int -> Int -> [Int]
genFib x1 x2 = x1:genFib x2 (x1+x2)

fibonacciOptimal:: [Int]
fibonacciOptimal = genFib 0 1
-- gen_fib 0 1
-- 0 gen_fib 1 (0+1)
-- 0:1 gen_fib 1 (1+1)
-- 0:1:1 gen_fib 2 (1+2)
-- 0:1:1:2 gen_fib 3 (2+3)
-- 0:1:1:2:3 gen_fib 5 (3+5)
-- 0:1:1:2:3:5 gen_fib 8 (5+8)

-- solução facil
fibonacci2:: [Int]
fibonacci2 = map fib [0..]

fib:: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- 2. Gere uma lista de infinitos primos
primes:: [Int]
primes = genPrimes [2..]

genPrimes:: [Int] -> [Int]
genPrimes (a:as) = a:genPrimes [x | x <- as, x `mod` a /= 0]

-- 3. Merge de duas listas ordenadas
merge:: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x<y = x:merge xs (y:ys)
                    | x >= y = y:merge (x:xs) ys

-- 4. Merge sort
mergeSort1:: [Int] -> [Int]
mergeSort1 [] = []
mergeSort1 (x:xs) = merge [x] (mergeSort1 xs)

mergeSort2:: [Int] -> [Int]
mergeSort2 = foldr (\ x -> merge [x]) []
