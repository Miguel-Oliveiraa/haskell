data Command = Forward Int | Backward Int | TurnLeft | TurnRight 
               deriving (Eq, Show, Read)

data Direction = North | South | West | East
                 deriving (Read, Show)

main = do
       a <- getLine
       b <- getLine
       let result = faces (read a) (read b)
       print result

faces :: Direction -> [Command] -> Direction
faces c [] = c
faces c d = faces (calcDirection c (head d)) (tail d)

calcDirection:: Direction -> Command -> Direction
calcDirection North TurnLeft = West
calcDirection North TurnRight = East
calcDirection North _ = North
calcDirection West TurnLeft = South  
calcDirection West TurnRight = North
calcDirection West _ = West
calcDirection East TurnLeft = North  
calcDirection East TurnRight = South
calcDirection East _ = East
calcDirection South TurnLeft = East
calcDirection South TurnRight = West
calcDirection South _ = South
