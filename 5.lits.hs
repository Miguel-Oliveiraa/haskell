-- 05. Listas e casamento de padrões
-- listas em haskell são linked lists
-- diferente de python onde listas são um vector/arraylist

-- : é o oprador construtor de lista
-- m:n m é a cabeça da lista e n é o elemento da cauda
listConstructor:: [Int]
listConstructor = 2:3:4:5:[]
-- Pegar um elemento da cabeça é O(1) e do fim O(n)

-- Declarando lista crescente
listSequence:: [Int]
listSequence = [1..20]

listSequenceChar:: [Char]
listSequenceChar = ['a'..'f']

listSequencePer2:: [Int]
listSequencePer2 = [0,2..100]

-- exemplos interessantes
-- ex1 =  [10,9..1] -- [10,9,8,7,6,5,4,3,2,1]
-- ex2 =  [10..1] -- []

-- funcoes sobre listas
a = head listSequence -- 1
b = tail listSequence -- [2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
c = length listSequence -- 20
d = listSequence ++ listSequencePer2 -- concat

-- somar lista
-- casamento de padrão
somarLista:: [Int] -> Int
somarLista [] = 0
somarLista (a:as) = a + somarLista(as)

-- recursao
somarListaRecursivo:: [Int] -> Int
somarListaRecursivo [] = 0
somarListaRecursivo a = (head a) + somarListaRecursivo (tail a)

-- ex1: quickSort
listaNaoOrdenada:: [Int]
listaNaoOrdenada = [7, 7, 2, 1, 13, 4, 8, 10, 20, 14, 25]

menoresQueN :: Int -> [Int] -> [Int]
menoresQueN _ [] = []
menoresQueN n (x:xs)
    | x < n     = x : menoresQueN n xs
    | otherwise = menoresQueN n xs

maioresQueN :: Int -> [Int] -> [Int]
maioresQueN _ [] = []
maioresQueN n (x:xs)
    | x > n     = x : maioresQueN n xs
    | otherwise = maioresQueN n xs

iguaisN:: Int -> [Int] -> [Int]
iguaisN _ [] = []
iguaisN n (x:xs)
    | x == n = x : iguaisN n xs
    | otherwise = iguaisN n xs

qSort :: [Int] -> [Int]
qSort [] = []
qSort a  = qSort left ++ equal ++ qSort right
  where
    pivot = head a
    left  = menoresQueN pivot a
    right = maioresQueN pivot a
    equal = iguaisN pivot a

-- ex2
fibonacci:: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci(n-1) + fibonacci(n-2)

fibonacciList :: Int -> [Int]
fibonacciList 0 = [0]
fibonacciList 1 = [1]
fibonacciList n =
    fibonacciList (n - 1)
    ++ last (fibonacciList (n - 1)) + last (fibonacciList (n - 2)):[]
