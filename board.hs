module Board where

createBoard :: [String] -> [[String]]
createBoard commandList = do
    let [width, height] = map (read :: String -> Int) $ tail commandList
    take height $ repeat $ replicate width " "

printBoard :: [[String]] -> IO ()
printBoard board = do
    mapM_ putStrLn $ map (\row -> "|" ++ (concat row) ++ "|") board
    let rowLength = length $ board !! 0
    putStrLn $ "+" ++ (replicate rowLength '-') ++ "+"

-- Returns the values at the given positions on the board
valuesAtPositions :: [[String]] -> [[Int]] -> [String]
valuesAtPositions board positions = map (\[x, y] -> board !! y !! x) positions

hasBlockAtPositions :: [[String]] -> [[Int]] -> Bool
hasBlockAtPositions board positions = any (/= " ") $ valuesAtPositions board positions
