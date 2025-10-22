-- fibonacci
auxFibb:: Int -> Int -> [Int]
auxFibb a b = a:auxFibb b (a+b)

fibonacci:: [Int]
fibonacci = auxFibb 0 1

takeFibonacci:: Int -> [Int]
takeFibonacci a = take a fibonacci

takeNthFibonacci:: Int -> Int
takeNthFibonacci a = head (take a fibonacci)

-- primo
primo:: Int -> Bool
primo x | x<2 = False
        | otherwise = primoAux x (x-1)

primoAux:: Int -> Int -> Bool
primoAux n 1 = True
primoAux n m | mod n m == 0 = False
             | otherwise = primoAux n (m-1)

primoEntreSi:: Int -> Int -> Bool
primoEntreSi a b | a > b = mdc a b == 1
                 | otherwise = mdc b a == 1

mdc:: Int -> Int -> Int
mdc a b | mod a b == 0 = b
        | otherwise = mdc b (mod a b)
        

-- fatorial
fatorial:: Int -> Int
fatorial 1 = 1
fatorial n = n * fatorial (n-1)

-- merge (mergeia duas listas ordenadas)
merge:: [Int] -> [Int] -> [Int]
merge [] [] = []
merge (a:as) [] = a:merge as []
merge [] (b:bs) = b:merge [] bs
merge (a:as) (b:bs) | a < b = a:merge as (b:bs)
                    | a == b = a:b: merge as bs
                    | otherwise = b:merge (a:as) bs
                    
mergeSort:: [Int] -> [Int]
-- mergeSort as = foldr (\x -> merge [x]) [] as
mergeSort [] = []
mergeSort (a:as) = merge [a] (mergeSort as)

-- quicksort
qsort:: [Int] -> [Int]
qsort [] = []
qsort (a:as) = qsort([x | x<-as, x<=a]) ++ [a] ++ qsort([x | x<-as, x>a])

-- tipos algebricos
-- tree
data Tree t = NilT |
              Node t (Tree t) (Tree t)
  deriving Show

depth:: Tree t -> Int
depth NilT = 0
depth (Node t a b) = 1 + max (depth a) (depth b) 

diameter:: Tree t -> Int
diameter NilT = 0
diameter (Node t a b) = 1 + depth a + depth b

preOrder:: Tree t -> [t]
preOrder NilT = []
preOrder (Node t a b) = [t] ++ preOrder a ++ preOrder b  


inOrder:: Tree t -> [t]
inOrder NilT = []
inOrder (Node t a b) = inOrder a ++ [t] ++ inOrder b  

posOrder:: Tree t -> [t]
posOrder NilT = []
posOrder (Node t a b) = posOrder a ++ posOrder b ++ [t]

-- pilha
type Pilha t = [t]
data Elemento = Valor Int | Soma | Multiplica deriving (Show)

-- gera string
geraString :: Pilha Elemento -> String
geraString as = head (foldl stringAux [] as)

stringAux:: [String] -> Elemento -> [String]
stringAux stack (Valor a) = show a:stack
stringAux (a:b:xs) Soma = ("(" ++ b ++ "+" ++ a ++ ")"):xs
stringAux (a:b:xs) Multiplica = ("(" ++ b ++ "*" ++ a ++ ")"):xs

-- calcula pilha
calcula :: Pilha Elemento -> Int
calcula as = head (foldl calculaAux [] as)

calculaAux:: [Int] -> Elemento -> [Int]
calculaAux stack (Valor a) = a:stack
calculaAux (a:b:xs) Soma = a + b:xs
calculaAux (a:b:xs) Multiplica = a*b:xs
