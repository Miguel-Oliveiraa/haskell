-- 12. Lazy Evaluation
-- Avaliação de uma expressão se dá apenas 
-- se e quando ser valor é necessário

f:: Int -> Int -> Int
f a b = a + b

resultado = f (9-3) (f 3 5)
-- (9-3) + (f 3 5)
-- 6 + (3+5)
-- 6 + 8
-- 14

f1:: Int -> Int -> Int
f1 a b = a + 12

resultado1 = f1 (9-3) (f1 3 5)
-- (9-3) + 12
-- 6 + 12
-- 18

g:: Int -> Int
g c = c + g c

ex1 = f1 3 (g 0)
ex2 = f1 (g 0) 3

troca :: Integer -> a -> a -> a
troca n x y
 | n > 0 = x
 | otherwise = y

f2:: [Int] -> [Int] -> Int
f2 (a:as) (b:bs) = a + b

exListaInfinita = f2 [1..] [2..]


