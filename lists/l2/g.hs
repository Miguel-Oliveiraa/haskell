data Tree t = Nilt |
              Node t (Tree t) (Tree t)
              deriving (Read)

main = do
       s <- getLine
       let result = maiorDiametro (read s::Tree Int)
       print result

alturaArvore :: Tree t -> Int
alturaArvore Nilt = 0
alturaArvore (Node t a b) = 1 + max (alturaArvore a) (alturaArvore b)  

maiorDiametro :: Tree t -> Int
maiorDiametro Nilt = 0
maiorDiametro (Node _ esquerda direita) = maximum [maiorDiametro esquerda, maiorDiametro direita, alturaArvore esquerda + alturaArvore direita + 1]