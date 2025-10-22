import Text.Read (Lexeme(String))
-- 1. escreva um codigo para gerar a lista infinita de fibonacci
-- fibonacci :: [Int]
-- Para testar, selecione os 20 primeiros números da lista e confira com a lista acima.

auxFibb:: Int -> Int -> [Int]
auxFibb x1 x2 = x1:auxFibb x2 (x1 + x2)

fibonacci:: [Int]
fibonacci = auxFibb 0 1

-- 2. recebe duas listas ordenadas e faz o merge
-- merge:: Ord t => [t] -> [t] -> [t]
merge:: Ord t => [t] -> [t] -> [t]
merge [] [] = []
merge (a:as) [] = a:merge as []
merge [] (b:bs) = b:merge [] bs
merge (a:as) (b:bs) | a < b = a:merge as (b:bs)
                    | a == b = a:b:merge as bs
                    | otherwise = b:merge (a:as) bs

-- 3. um algoritmo para merge sort
mergeSort:: Ord t => [t] -> [t]
mergeSort (a:as) = foldr (\x -> merge [x]) [a] as 

-- 4.  Escreva uma funcao que recebe uma pilha de elementos e retorna a string
type Pilha t = [t]
exemploPilhaElem :: Pilha Elemento
exemploPilhaElem = [Valor 10, Valor 20, Soma, Valor 30, Multiplica]
-- exemplo de uso: gera_string exemploPilhaElem ——> "((10+20)*30)"
data Elemento = Valor Int | Soma | Multiplica deriving (Show)

geraString :: Pilha Elemento -> String
geraString pilha = head (foldl processaElemento [] pilha)
  where
  processaElemento :: [String] -> Elemento -> [String]
  processaElemento stack (Valor n) = show n : stack
  processaElemento (s1:s2:ss) Soma = ("(" ++ s2 ++ "+" ++ s1 ++ ")") : ss
  processaElemento (s1:s2:ss) Multiplica = ("(" ++ s2 ++ "*" ++ s1 ++ ")") : ss

-- 5. Escreva uma funcao que retorna o resultado da pilha
-- exemplo de uso: calcula exemploPilhaElem ——> 900
calcula :: Pilha Elemento -> Int
calcula pilha = head (foldl calculaElemento [] pilha)
  where
  calculaElemento :: [Int] -> Elemento -> [Int]
  calculaElemento stack (Valor n) = n : stack
  calculaElemento (s1:s2:ss) Soma = (s1 + s2):ss
  calculaElemento (s1:s2:ss) Multiplica = (s1 * s2):ss
  