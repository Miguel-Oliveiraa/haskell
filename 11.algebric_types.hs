-- 11. Tipos algebricos
-- 1. Tipos Enumerados "Enum"
data Estacao = Inverno | Verao | Outono | Primavera
  deriving Show
  -- necessario usar "deriving Show" para printar
data Temp = Frio | Quente
  deriving Show

clima:: Estacao -> Temp
clima Inverno = Frio
clima _ = Quente

-- 2. Tipo Produto "X"
type Nome = String
type Idade = Int
data Pessoa = Pessoa Nome Idade

jose = Pessoa "Jose" 22
maria = Pessoa "Maria" 23

showPerson :: Pessoa -> String
showPerson (Pessoa n i) = n ++ " -- " ++ show i

-- Construtores com argumentos
data Shape = Circle Float
              | Rectangle Float Float
              | Square Float
    deriving Show

circulo :: Shape
circulo = Circle 2.3
retangulo :: Shape
retangulo = Rectangle 2.0 4.0
quadrado:: Shape
quadrado = Square 2.0

isRound :: Shape -> Bool
isRound (Circle _) = True
isRound (Rectangle _ _) = False

area::Shape -> Float
area (Circle r) = pi * r*r
area (Rectangle l1 l2) = l1 * l2
area (Square l) = l*l

-- 3. Tipo recurso
data Expr = Lit Int |
                Add Expr Expr |
                Sub Expr Expr |
                Mult Expr Expr
eval:: Expr -> Int
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Mult e1 e2) = eval e1 * eval e2


-- Questoes
-- 1. showExpr :: Expr -> String
showExpr :: Expr -> String
showExpr (Lit n) = show n
showExpr (Add e1 e2) = "(" ++ showExpr  e1 ++ ")" ++ "+" ++ "(" ++ showExpr e2 ++ ")"
showExpr (Sub e1 e2) = "(" ++ showExpr  e1 ++ ")" ++ "-" ++ "(" ++ showExpr e2 ++ ")"
showExpr (Mult e1 e2) = "(" ++ showExpr  e1 ++ ")" ++ "*" ++ "(" ++ showExpr e2 ++ ")"

expressao = Sub (Add (Lit 3) (Lit 4)) (Add (Lit 9) (Lit 10))

-- 2. toList :: List t -> [t]
data List t = Nil | Cons t (List t)
  deriving Show
exemploLista = Cons 3 (Cons 2 Nil)

toList :: List t -> [t]
toList Nil = []
toList (Cons x list) = x:toList list

-- 3. fromList :: [t] -> List t
fromList :: [t] -> List t
fromList [] = Nil
fromList (a:as) = Cons a (fromList as)

-- 4. depth :: Tree t -> Int
data Tree t = NilT |
              Node t (Tree t) (Tree t)
  deriving Show

arvore = Node 1 (Node 2 NilT NilT) (Node 3 NilT (Node 1 NilT NilT))               
depth :: Tree t -> Int
depth NilT = 0
depth (Node a tree1 tree2) = 1 + max (depth tree1) (depth tree2)


-- 5. collapse :: Tree t -> [t]
-- 6. mapTree :: (t -> u) -> Tree t -> Tree u