-- 09. Funções de alta ordem: mapeamento, redução, filtragem 

-- recebe funcao como parametro
applyBinOper:: (t -> t -> t) -> t -> t -> t
applyBinOper f x y = f x y

-- Map
lista = [1..10]
squares1 xs = map (\x -> x*x) xs

-- Pode ser implementado por Compreensões de lista
squares2 xs = [x*x|x <- xs]

-- Reduce (foldr, fold because it's like folding a paper page to transform it into something simpler).
-- always use foldr, because it's fold from the right to left, and doesn't stack data in memory
-- foldr is better for lazy evaluation
concatList :: [[a]] -> [a]
concatList a = foldr (++) [] a

sumList:: Num a => [a] -> a
sumList xs = foldr (+) 0 xs

--  Filter
evenNumbers1:: Integral a => [a] -> [a]
evenNumbers1 xs = filter (\x-> (mod x 2) == 0) xs

-- Pode ser implementado por Compreensões de lista
evenNumbers2:: Integral a => [a] -> [a]
evenNumbers2 xs = [x | x <- xs, mod x 2 == 0]

-- ex1
-- Implemente funções takeWhile e
-- dropWhile:

-- > takeWhile (/= ' ') "The Office"
-- "The"
-- > dropWhile (< 42) [1,5,6,25,64,13]
-- [64,13]

takeWhile1:: (a -> Bool) -> [a] -> [a]
takeWhile1 f (x:xs) | f x = x:takeWhile1 f xs
                    | otherwise = []

dropWhile1:: (a -> Bool) -> [a] -> [a]
dropWhile1 f (x:xs) | f x = dropWhile1 f xs
                    | otherwise = x:xs  