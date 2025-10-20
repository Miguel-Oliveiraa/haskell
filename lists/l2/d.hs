data Tree t = Node t (Tree t) (Tree t) 
              | Nilt
              deriving (Read)

main = do
       a <- getLine
       let result = alturaArvore (read a::Tree Int)
       print result

alturaArvore :: Tree t -> Int
alturaArvore Nilt = 0
alturaArvore (Node t a b) = 1 + max (alturaArvore a) (alturaArvore b)  
