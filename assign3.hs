module Main where

-- Import necessary libraries
import System.IO
import Data.List
import Data.String
import Test.HUnit

-- Import local modules
import qualified Board
import qualified Tetrimino
import qualified Moves

-- Main function
main :: IO ()
main = do
    input <- getContents
    putStr $ show $ readFrom input

readFrom :: String -> String
readFrom input = do
    let commands = parseCommands input
    let board = Board.createBoard $ commands !! 0
    let dice =  createDice $ commands !! 1
    let moves = createMoves $ commands !! 2
    Board.formatBoard board 0

-- Parses a string with all the commands
-- Returns a 2D list of strings where each sublist is each string in the command
parseCommands :: String -> [[String]]
parseCommands inputString = map (words) $ lines inputString

-- Creates an endless sequence for the dice
createDice :: [String] -> [Int]
createDice commandList = cycle $ map (read :: String -> Int) $ tail commandList

-- Returns the moves as a string where each char is a move
createMoves :: [String] -> String
createMoves commandList = concat $ tail commandList

tests = test [
--  testSimpleEmpty
	 "tiny empty board" ~:
              "|       | 0 pieces\n\
              \|       |\n\
              \|       |\n\
              \|       |\n\
              \+-------+\n" ~=?
            (show $ readFrom "board 7 4")

	,"larger empty board" ~:
              "|        | 0 pieces\n\
              \|        |\n\
              \|        |\n\
              \|        |\n\
              \|        |\n\
              \|        |\n\
              \+--------+\n\
              \" ~=?
            (show $ readFrom "board 8 6")

	,"odd-size board with powerups" ~:
              "|       | 0 pieces\n\
              \|  x    |\n\
              \|       |\n\
              \| x     |\n\
              \+-------+\n\
              \" ~=?
            (show $ readFrom "board 7 4\n\
                             \powerup 3 3\n\
                             \powerup 2 1")

--  testSimpleEvenWidth
	,"podium" ~:
              "|   p    | 1 piece\n\
              \|  ppp   |\n\
              \|        |\n\
              \|        |\n\
              \|        |\n\
              \|        |\n\
              \+--------+\n\
              \" ~=?
            (show $ readFrom "board 8 6\n\
                             \dice 6 1\n\
                             \moves")

	,"podium rotated" ~:
              "|   p    | 1 piece\n\
              \|   pp   |\n\
              \|   p    |\n\
              \|        |\n\
              \|        |\n\
              \|        |\n\
              \+--------+\n\
              \" ~=?
            (show $ readFrom "board 8 6\n\
                             \dice 6 2\n\
                             \moves")

	,"green piece" ~:
              "|   gg   | 1 piece\n\
              \|  gg    |\n\
              \|        |\n\
              \|        |\n\
              \|        |\n\
              \|        |\n\
              \+--------+\n\
              \" ~=?
            (show $ readFrom "board 8 6\n\
                             \dice 3 1\n\
                             \moves")

	,"green rotated" ~:
              "|   g    | 1 piece\n\
              \|   gg   |\n\
              \|    g   |\n\
              \|        |\n\
              \|        |\n\
              \|        |\n\
              \+--------+\n\
              \" ~=?
            (show $ readFrom "board 8 6\n\
                             \dice 3 2\n\
                             \moves ")

         ,"upside-w" ~:
               "|   gg   | 1 piece\n\
               \|  gg    |\n\
               \|        |\n\
               \|        |\n\
               \|        |\n\
               \|        |\n\
               \+--------+\n\
               \" ~=?
             (show $ readFrom "board 8 6\n\
                             \dice 3 3\n\
                             \moves R")

	,"green can rotate" ~:
              "|   g    | 1 piece\n\
              \|   gg   |\n\
              \|    g   |\n\
              \|        |\n\
              \|        |\n\
              \|        |\n\
              \+--------+\n\
              \" ~=?
            (show $ readFrom "board 8 6\n\
                             \dice 3 1\n\
                             \moves R")

	,"2 pieces" ~:
              "|   c    | 2 pieces\n\
              \|   c    |\n\
              \|   c    |\n\
              \|   c    |\n\
              \|        |\n\
              \|        |\n\
              \|        |\n\
              \|        |\n\
              \|   yy   |\n\
              \|   yy   |\n\
              \+--------+\n\
              \" ~=?
            (show $ readFrom "board 8 10\n\
                             \dice 1 2 7 2 2\n\
                             \moves +")

	,"cyan unrotated" ~:
              "|  cccc  | 2 pieces\n\
              \|        |\n\
              \|        |\n\
              \|        |\n\
              \|        |\n\
              \|        |\n\
              \|        |\n\
              \|        |\n\
              \|   yy   |\n\
              \|   yy   |\n\
              \+--------+\n\
              \" ~=?
            (show $ readFrom "board 8 10\n\
                             \dice 1 1 7 1 2\n\
                             \moves +")

	,"3 pieces" ~:
              "|  rr    | 3 pieces\n\
              \|   rr   |\n\
              \|        |\n\
              \|        |\n\
              \|   c    |\n\
              \|   c    |\n\
              \|   c    |\n\
              \|   c    |\n\
              \|   yy   |\n\
              \|   yy   |\n\
              \+--------+\n\
              \" ~=?
            (show $ readFrom "board 8 10\n\
                             \dice 1 2 7 2 2\n\
                             \moves ++")

	,"2 pieces with powerup" ~:
              "|        | 2 pieces\n\
              \| x      |\n\
              \|   c    |\n\
              \|   c    |\n\
              \|   c    |\n\
              \|   c    |\n\
              \|        |\n\
              \|        |\n\
              \|yy      |\n\
              \|yy      |\n\
              \+--------+\n\
              \" ~=?
            (show $ readFrom "board 8 10\n\
                             \powerup 2 9\n\
                             \dice 1 2 7 2 2\n\
                             \moves ll ll\n\
                             \moves +..")

	,"2 pieces with powerup hidden" ~:
              "|        | 2 pieces\n\
              \| c      |\n\
              \| c      |\n\
              \| c      |\n\
              \| c      |\n\
              \|        |\n\
              \|        |\n\
              \|        |\n\
              \|yy      |\n\
              \|yy      |\n\
              \+--------+\n\
              \" ~=?
            (show $ readFrom "board 8 10\n\
                             \powerup 2 9\n\
                             \dice 1 2 7 2 2\n\
                             \moves ll ll\n\
                             \moves +ll.")


--  testSimpleOddWidth
	,"odd green" ~:
              "|   gg  | 1 piece\n\
              \|  gg   |\n\
              \|       |\n\
              \|       |\n\
              \|       |\n\
              \|       |\n\
              \+-------+\n\
              \" ~=?
            (show $ readFrom "board 7 6\n\
                             \dice 3 1\n\
                             \moves")

	,"odd green rotated" ~:
              "|   g   | 1 piece\n\
              \|   gg  |\n\
              \|    g  |\n\
              \|       |\n\
              \|       |\n\
              \|       |\n\
              \+-------+\n\
              \" ~=?
            (show $ readFrom "board 7 6\n\
                             \dice 3 2\n\
                             \moves ")

	,"odd 2 pieces" ~:
              "|   c   | 2 pieces\n\
              \|   c   |\n\
              \|   c   |\n\
              \|   c   |\n\
              \|       |\n\
              \|       |\n\
              \|       |\n\
              \|       |\n\
              \|   yy  |\n\
              \|   yy  |\n\
              \+-------+\n\
              \" ~=?
            (show $ readFrom "board 7 10\n\
                             \dice 1 2 7 2 2\n\
                             \moves +")

	,"odd 2 cyan horizontal" ~:
              "|  cccc | 2 pieces\n\
              \|       |\n\
              \|       |\n\
              \|       |\n\
              \|       |\n\
              \|       |\n\
              \|       |\n\
              \|       |\n\
              \|   yy  |\n\
              \|   yy  |\n\
              \+-------+\n\
              \" ~=?
            (show $ readFrom "board 7 10\n\
                             \dice 1 1 7 1 2\n\
                             \moves +")

	,"odd 3 pieces" ~:
              "|  rr   | 3 pieces\n\
              \|   rr  |\n\
              \|       |\n\
              \|       |\n\
              \|   c   |\n\
              \|   c   |\n\
              \|   c   |\n\
              \|   c   |\n\
              \|   yy  |\n\
              \|   yy  |\n\
              \+-------+\n\
              \" ~=?
            (show $ readFrom "board 7 10\n\
                             \dice 1 2 7 2 2\n\
                             \moves ++")


--  testMultipiece
	,"4 pieces" ~:
              "|   gg  | 4 pieces\n\
              \|  gg   |\n\
              \|   c   |\n\
              \|   c   |\n\
              \|   c   |\n\
              \|   c   |\n\
              \|  rr   |\n\
              \|   rr  |\n\
              \|   gg  |\n\
              \|  gg   |\n\
              \+-------+\n\
              \" ~=?
            (show $ readFrom "board 7 10\n\
                             \dice 3 1 2 1 7 2\n\
                             \moves +++++++++++")

	,"20 pieces" ~:
              "|        | 20 pieces\n\
              \|        |\n\
              \|   c    |\n\
              \|   c    |\n\
              \|   c    |\n\
              \|   c    |\n\
              \|   c    |\n\
              \|   c    |\n\
              \|   c    |\n\
              \|   c    |\n\
              \+--------+\n\
              \" ~=?
            (show $ readFrom "board 8 10\n\
                             \dice 7 2\n\
                             \moves r+ rr+ rrr+ rrrr+ l+ ll+ lll+ Rrrrrr+ rrrr+ rrrr+ rrrr+ rrrr+ + Rllll+ lll+ ll+ l+  +++++++")

	,"14 pieces" ~:
              "|   c    | 14 pieces\n\
              \|   c    |\n\
              \|   c    |\n\
              \|  xc    |\n\
              \|   c    |\n\
              \|   c    |\n\
              \|   c    |\n\
              \|cccc    |\n\
              \|cccc    |\n\
              \|cccc    |\n\
              \+--------+\n\
              \" ~=?
            (show $ readFrom "board 8 10\n\
                             \dice 7 2\n\
                             \powerup 3 7\n\
                             \moves r+ rr+ rrr+ rrrr+ l+ ll+ lll+ l....r +Rrrrr+lll+ll+l++++")
             ]
