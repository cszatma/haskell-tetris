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

valuesAtPositions :: [[String]] -> [[Int]] -> [String]
valuesAtPositions board positions = map (\[row, col] -> board !! row !! col) positions

hasBlockAtPositions :: [[String]] -> [[Int]] -> Bool
hasBlockAtPositions board positions = not $ null $ filter (/= " ") $ valuesAtPositions board positions 
