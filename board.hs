module Board where

type Board = [[String]]

createBoard :: [String] -> Board
createBoard commandList = do
    let [width, height] = map (read :: String -> Int) $ tail commandList
    take height $ repeat $ replicate width " "

printBoard :: Board -> IO ()
printBoard board = do
    mapM_ putStrLn $ map (\row -> "|" ++ (concat row) ++ "|") board
    let rowLength = length $ board !! 0
    putStrLn $ "+" ++ (replicate rowLength '-') ++ "+"

-- Returns the values at the given positions on the board
valuesAtPositions :: Board -> [[Int]] -> [String]
valuesAtPositions board positions = map (\[x, y] -> board !! y !! x) positions

hasBlockAtPositions :: Board -> [[Int]] -> Bool
hasBlockAtPositions board positions = any (/= " ") $ valuesAtPositions board positions

numOfRows :: Board -> Int
numOfRows board = length $ board !! 0

numOfCols :: Board -> Int
numOfCols board = length board
