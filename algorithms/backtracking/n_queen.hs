solveNQueens :: Int -> [[String]]
solveNQueens n =
    let solutions = solve n 0 []
    in map (normalize n . reverse) solutions


solve :: Int -> Int -> [Int] -> [[Int]]
solve n r cols
    -- base (if r = n, means we put queens in all the lines)
    | r == n    = [cols]
    -- recursion: search a position in line r
    | otherwise =
        -- find all safe columns in the line
        let safeCols = filter (isSafe cols) [0..n-1]
        
        -- for each safe columns, calls solve in (r+1) and add the queen to array of solutions
        in concatMap (\c -> solve n (r + 1) (c : cols)) safeCols

isSafe :: [Int] -> Int -> Bool
isSafe cols newCol =
    -- queen is in line: lenght cols
    let r = length cols
    
        -- Create tuples for all the existing queens
        indexedCols = zip [r-1, r-2 .. 0] cols
        
    -- verify if the queen is attacked in all last coluns
    in all (\(r', c') ->
            newCol /= c' &&                 -- same column
            abs (r - r') /= abs (newCol - c') -- same diagonal
          ) indexedCols

-- | print function (idk AI created this)
normalize :: Int -> [Int] -> [String]
normalize n cols = map buildRow cols
  where
    buildRow queenCol = [ if c == queenCol then 'Q' else '.' | c <- [0..n-1] ]

