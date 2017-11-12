module Board where

createBoard list = do
    let [width, height] = map (read :: String -> Int) $ tail list
    take height $ repeat $ take width $ repeat " "     

printBoard board = do
    mapM_ putStrLn $ map (\row -> "|" ++ (concat row) ++ "|") board
    let rowLength = length $ board !! 0
    putStrLn $ "+" ++ (replicate rowLength '-') ++ "+"
