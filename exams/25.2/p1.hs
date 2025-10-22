-- 1 questao - resolvida
votos:: [Int]
votos = [1,2,1,9,99,9,8,2,2,9,1,7,0,1,8,8,8,8,7]

winner:: [Int] -> Int
winner as = snd (biggerTuple (winnerAux as)) 

biggerTuple:: [(Int,Int)] -> (Int,Int)
biggerTuple (a:[]) = a
biggerTuple (a:b:as) = max (max a b) (biggerTuple as)

winnerAux:: [Int] -> [(Int, Int)]
winnerAux [] = []
winnerAux (a:as) = (length [x | x <- as, x == a], a):winnerAux [x | x <- as, x /= a]

-- 2 questao - resolvida
unicos [] = []
unicos (a:as) = a:unicos [x | x <- as, x /= a]

-- 3 questao - resolvida
data ExpB = OR ExpB ExpB | AND ExpB ExpB | NOT ExpB | VAR Char
    deriving Show

exemplo1:: ExpB
exemplo1 = OR (AND (VAR 'x') (VAR 'y')) (NOT (VAR 'y'))

vars:: ExpB -> [Char]
vars as = unicos (varsAux as) 

varsAux:: ExpB -> [Char]
varsAux (OR a b) = varsAux a ++ varsAux b
varsAux (AND a b) = varsAux a ++ varsAux b
varsAux (NOT a) = varsAux a 
varsAux (VAR a) = [a]

-- 4 questao - resolvida
values1 = [('x', True), ('y',False)] 
values2 = [('x', False), ('y',True)] 

eval:: [(Char, Bool)] -> ExpB -> Bool
eval values (OR a b) = or ((eval values a):(eval values b):[])
eval values (AND a b) = and ((eval values a):(eval values b):[])
eval values (NOT a) = not (eval values a) 
eval values (VAR a) = head [snd x | x<-values, fst x == a] 

