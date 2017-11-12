module Board where

createBoard list = do
    map (read :: String -> Int) $ tail list
