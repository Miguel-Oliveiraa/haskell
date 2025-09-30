main = do
       a <- getLine
       let result = bSort (read a :: [String])
       print result

bSort :: [String] -> [String]
bSort [] = []
bSort list = bSort[x | x <-list, x<head list] ++ [x | x<-list, x == head list] ++ bSort[x | x <-list, x>head list]