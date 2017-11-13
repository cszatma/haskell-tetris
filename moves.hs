module Moves where

import qualified Board
import qualified Tetrimino

data Status = Ok | Failed | Placed deriving (Eq, Enum, Show)

performMove :: Board.Board -> Tetrimino.Tetrimino -> Char -> (Tetrimino.Tetrimino, Status)
performMove board tetrimino move
    | move == 'l' || move == 'r' = shiftHorizontally board tetrimino move
    | move == '.'                = shiftDown board tetrimino
    | move == '+'                = performDrop board tetrimino

-- Shifts a tetrimino either left or right
shiftHorizontally :: Board.Board -> Tetrimino.Tetrimino -> Char -> (Tetrimino.Tetrimino, Status)
shiftHorizontally board tetrimino shift = do
    let (x, y) = Tetrimino.position tetrimino
    let shiftedTetrimino = tetrimino { Tetrimino.position = (x + (getShiftValue shift), y) }
    if pieceDoesOverlap board tetrimino then (tetrimino, Failed)
    else (shiftedTetrimino, Ok)

-- Shifts a tetrimino down by 1
shiftDown :: Board.Board -> Tetrimino.Tetrimino -> (Tetrimino.Tetrimino, Status)
shiftDown board tetrimino = do
    if isOnBottom board tetrimino then (tetrimino, Placed)
    else do
        let (x, y) = Tetrimino.position tetrimino
        let shiftedTetrimino = tetrimino { Tetrimino.position = (x, y + 1) }
        if pieceDoesOverlap board tetrimino then (tetrimino, Placed)
        else (shiftedTetrimino, Ok)

-- Shifts a tetrimino down until it either reaches the bottom of the board or collides with another tetrimino
performDrop :: Board.Board -> Tetrimino.Tetrimino -> (Tetrimino.Tetrimino, Status)
performDrop board tetrimino = do
    let (newTetrimino, status) = shiftDown board tetrimino
    if status == Ok then performDrop board newTetrimino
    else (newTetrimino, Placed)

-- Checks if the tetrimino is at the bottom of the board
isOnBottom :: Board.Board -> Tetrimino.Tetrimino -> Bool
isOnBottom board tetrimino = elem (length board - 1) $ Tetrimino.getYValues tetrimino

-- Checks if a tetrimino overlaps with another tetrimino on the board
pieceDoesOverlap :: Board.Board -> Tetrimino.Tetrimino -> Bool
pieceDoesOverlap board tetrimino = Board.hasBlockAtPositions board $ Tetrimino.getPosOnBoard tetrimino

-- Checks if the tetrimino has gone past either the left or right side of the board
isOutOfBounds :: Board.Board -> Tetrimino.Tetrimino -> Bool
isOutOfBounds board tetrimino
    | any (== (-1)) xValues      = True
    | any (== rowCount) xValues  = True
    | otherwise                  = False
    where xValues = Tetrimino.getXValues tetrimino
          rowCount = Board.numOfRows board

getShiftValue :: Char -> Int
getShiftValue 'l' = -1

getShiftValue 'r' = 1
