module Moves where

import qualified Board
import qualified Tetrimino
 
performMove board tetrimino move
    | move == "l" || move == "r" = shiftHorizontally board tetrimino move
    | move == "." = shiftDown board tetrimino
    | move == "+" = performDrop board tetrimino

shiftHorizontally board tetrimino move = 100

shiftDown board tetrimino = 101

performDrop board tetrimino = 102

-- Checks if the tetrimino is at the bottom of the board
isOnBottom :: [[String]] -> Tetrimino.Tetrimino -> Bool
isOnBottom board tetrimino = elem (length board - 1) $ Tetrimino.getYValues tetrimino

-- Checks if a tetrimino overlaps with another tetrimino on the board
pieceDoesOverlap :: [[String]] -> Tetrimino.Tetrimino -> Bool
pieceDoesOverlap board tetrimino = Board.hasBlockAtPositions board $ Tetrimino.getPosOnBoard tetrimino
