data Command = Forward Int | Backward Int | TurnLeft | TurnRight 
               deriving (Eq, Show, Read)

data Direction = N | O | L | S
                 deriving (Eq, Show, Read)
-- Norte Oeste Lest Sul

main = do
       a <- getLine
       b <- getLine
       let result = destination (read a) (read b)
       print result

destination :: (Int,Int) -> [Command] -> (Int,Int) 
destination coord as = snd (helper N coord as)

helper:: Direction -> (Int, Int) -> [Command] -> (Direction, (Int,Int))
helper d coord [] = (d, coord)
helper d coord (a:as) = helper (fst (calcStep d coord a)) (snd (calcStep d coord a)) as


-- fst
-- snd
calcStep:: Direction -> (Int, Int) -> (Command) -> (Direction, (Int,Int))
calcStep N coord TurnLeft = (L,coord) 
calcStep N coord TurnRight = (O,coord)
calcStep N (a,b) (Forward n) = (N,(a,b+n)) 
calcStep N (a,b) (Backward n) = (N,(a,b-n)) 
calcStep L coord TurnLeft = (S,coord) 
calcStep L coord TurnRight = (N,coord) 
calcStep L (a,b) (Forward n) = (L,(a-n,b)) 
calcStep L (a,b) (Backward n) = (L,(a+n,b)) 
calcStep O coord TurnLeft = (N,coord) 
calcStep O coord TurnRight = (S,coord) 
calcStep O (a,b) (Forward n) = (O,(a+n,b)) 
calcStep O (a,b) (Backward n) = (O,(a-n,b)) 
calcStep S coord TurnLeft = (O,coord) 
calcStep S coord TurnRight = (L,coord) 
calcStep S (a,b) (Forward n) = (S,(a,b-n)) 
calcStep S (a,b) (Backward n) = (S,(a,b+n)) 
