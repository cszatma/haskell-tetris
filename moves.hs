module Moves where
 
performMove board tetrimino move
    | move == "l" || move == "r" = shiftHorizontally board tetrimino move
    | move == "." = shiftDown board tetrimino
    | move == "+" = performDrop board tetrimino

shiftHorizontally board tetrimino move = 100

shiftDown board tetrimino = 101

performDrop board tetrimino = 102
