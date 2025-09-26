-- funcoes e variaveis
vendas :: Int -> Int
vendas 0 = 10
vendas 1 = 20
vendas 2 = 30
vendas 3 = 40
vendas 4 = 50
vendas 5 = 15
vendas 6 = 15
vendas 7 = 35

lista = [1..10]
---

vendasIguais :: Int -> Int -> Int
vendasIguais valor 0 | vendas 0 == valor = 1
                     | otherwise = 0 
vendasIguais valor semana | vendas semana == valor = 1 + vendasIguais valor (semana-1)
                          |otherwise = 0 + vendasIguais valor (semana -1)

primo :: Int -> Bool
primo n | n < 2 = False
        | otherwise = primoAux n (n-1)

        
primoAux :: Int -> Int -> Bool
primoAux n 1 = True
primoAux n m | n `mod` m == 0 = False
             | otherwise = primoAux n (m-1)

-------

primoEntreSi :: Int -> Int -> Bool
primoEntreSi m n | m < n = mdc m n == 1
                 | otherwise = mdc n m == 1

mdc:: Int -> Int -> Int
mdc m n | m `mod` n == 0 = n
        | otherwise = mdc n (m `mod` n)

------------------

fat:: Int -> Int
fat 0 = 0
fat x = x + fat (x-1)

------------------
allEqual:: Int -> Int -> Int -> Bool
allEqual m n p = m == n && n == p

all4Equal:: Int -> Int -> Int -> Int -> Bool
all4Equal m n p q = allEqual m n p && m == q