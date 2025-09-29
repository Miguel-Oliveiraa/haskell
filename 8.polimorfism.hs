-- 08. Funções polimórficas e classes de tipos.
-- funções polimórficas funcionam com qualquer tipo de dado.

-- 1. polimorfismo paramétrico
-- A função 'primeiro' é polimórfica porque pode aceitar uma lista de qualquer tipo

-- Exemplo: função que retorna o primeiro elemento de uma lista.
primeiro :: [a] -> a
primeiro (x:_) = x

-- repete valor x n vezes
rep ch 0 = []
rep ch n = ch : rep ch (n-1)

-- 2. Polimorfismo sobrecarga
-- Aceite mais de um tipo de dado, mas com restrições.
-- De acordo com uma classe de tipos.

-- Classes de tipos: Eq, Ord, Show, Read, Num, Integral, Fractional
-- Classes são coleções de tipos que compartilham certas operações.

-- Como ver um tipo e suas instâncias no GHCi:
-- ghci> :i Char
-- type Char :: *
-- data Char = GHC.Types.C# GHC.Prim.Char#
--         -- Defined in `GHC.Types'
-- instance Read Char -- Defined in `GHC.Read'
-- instance Bounded Char -- Defined in `GHC.Enum'
-- instance Enum Char -- Defined in `GHC.Enum'
-- instance Ord Char -- Defined in `GHC.Classes'
-- instance Show Char -- Defined in `GHC.Show'
-- instance Eq Char -- Defined in `GHC.Classes'

-- Podemos ver uma classe, suas operações e tipos que ela aceita:
-- ghci> :i Eq
-- type Eq :: * -> Constraint
-- class Eq a where
--   (==) :: a -> a -> Bool
--   (/=) :: a -> a -> Bool
--   {-# MINIMAL (==) | (/=) #-}


-- Exemplo: função que verifica se um elemento está em uma lista.
pertence :: Eq a => a -> [a] -> Bool
pertence _ [] = False
pertence x (y:ys) = x == y || pertence x ys

-- ex1:
-- agrupar:: Eq a => [[a]] -> [(a,Int)]
-- agrupar [a:as] = (a,contar a as)

contar:: Eq a => a -> [a] -> Int
contar x [] = 0
contar x (a:as) | x == a = 1 + contar x as
                | otherwise = contar x as

agrupar:: Eq a => [[a]] ->[(a, Int)]
agrupar xs = [(x, contar x (concat xs))| x <- removeDup (concat xs)]
  where removeDup [] = []
        removeDup (x:xs) = x : filter (/=x) (removeDup xs)


