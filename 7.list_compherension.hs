-- 07. Compreensões de listas
lista:: [Int]
lista = [1..10]

-- [expressa | variavel <- lista, cond]
listaQuadrado:: [Int] -> [Int]
listaQuadrado lista = [x*x | x <- lista]

listaPares:: [Int] -> [Int]
listaPares lista = [x | x <- lista, mod x 2 == 0]

-- ex1: quickSort com compreensao de lista
listaNaoOrdenada:: [Int]
listaNaoOrdenada = [7, 7, 2, 1, 13, 4, 8, 10, 20, 14, 25]

qSort:: [Int] -> [Int]
qSort [] = []
qSort a = qSort([x | x<-a, x<head (a)]) ++ [x | x<-a, x == head(a)] ++ qSort([x | x<-a, x>head (a)])

-- ex2: Operando um "banco de dados"
type Pessoa = String
type Livro = String
type BancoDados = [(Pessoa,Livro)]

baseExemplo:: BancoDados
baseExemplo = [("Sergio","O Senhor dos Aneis"), ("Andre","Duna"), ("Fernando","Jonathan Strange & Mr. Norrell"), ("Fernando","A Game of Thrones")] -- livros emprestados

-- livros que uma pessoa pegou emprestado
livros:: BancoDados -> Pessoa -> [Livro] 
livros banco alvo = [livro | (pessoa,livro)<- banco, pessoa == alvo]
emprestimos:: BancoDados -> Livro ->[Pessoa]
emprestimos banco alvo = [pessoa | (pessoa, livro) <- banco, livro == alvo]
emprestado:: BancoDados -> Livro -> Bool
emprestado banco alvo = length [livro | (pessoa, livro) <-banco, livro == alvo] > 0
qtdEmprestimos:: BancoDados -> Pessoa -> Int
qtdEmprestimos banco alvo = length [livro | (pessoa, livro) <-banco, livro == alvo]
emprestar:: BancoDados -> Pessoa -> Livro -> BancoDados
emprestar [] p l = [(p, l)]
emprestar ((p,l):as) pessoa livro 
  | p == pessoa && l == livro = ((p,l):as)
  | otherwise                 = (p,l):emprestar as pessoa livro
  
devolver:: BancoDados -> Pessoa -> Livro -> BancoDados
devolver [] p l = []
devolver ((p,l):as) pessoa livro
  | p == pessoa && l == livro = as
  | otherwise                 = (p,l):devolver as pessoa livro