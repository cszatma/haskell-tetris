module Tetrimino where

-- Define Tetrimino type
data Tetrimino = Tetrimino { points :: [[Int]], position :: (Int, Int), typeString :: String } deriving (Show)

-- Creating Tetriminos
createType :: Int -> (Int, Int) -> Tetrimino
createType 1 pos = Tetrimino [[0, 0], [1, 0], [0, -1], [1, -1]] pos "y"

createType 2 pos = Tetrimino [[-1, -1], [0, -1], [0, 0], [1, 0]] pos "r"

createType 3 pos = Tetrimino [[-1, 0], [0, 0], [0, -1], [1, -1]] pos "g"

createType 4 pos = Tetrimino [[-1, -1], [-1, 0], [0, 0], [1, 0]] pos "b"

createType 5 pos = Tetrimino [[-1, 0], [0, 0], [1, 0], [1, -1]] pos "o"

createType 6 pos = Tetrimino [[-1, 0], [0, 0], [1, 0], [0, -1]] pos "p"

createType 7 pos = Tetrimino [[-1, 0], [0, 0], [1, 0], [2, 0]] pos "c"

-- Returns the absolute positions of the tetrimino
getPosOnBoard :: Tetrimino -> [[Int]]
getPosOnBoard (Tetrimino points pos _) = map (\point -> [point !! 0 + fst pos, point !! 1 + snd pos]) points

-- Returns all the absolute X values of the tetrimino
getXValues :: Tetrimino -> [Int]
getXValues tetrimino = map (!! 0) $ getPosOnBoard tetrimino

-- Returns all the absolute Y values of the tetrimino
getYValues :: Tetrimino -> [Int]
getYValues tetrimino = map (!! 1) $ getPosOnBoard tetrimino
