module Game where

import qualified Board
import qualified Tetrimino

runGame :: Board.Board -> [Int] -> [String] -> Board.Board
runGame board dice moves = runMoves board dice moves 0

runMoves :: Board.Board -> [Int] -> [String] -> Int -> Board.Board
runMoves board (currentRoll:remainingRolls) (currentMove:remainingMoves) numOfPieces = board
