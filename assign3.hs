module Main where

-- Import necessary libraries
import System.IO
import Data.List
import Data.String

-- Import local modules
import qualified Board
import qualified Tetrimino
import qualified Moves

-- Main function
main :: IO ()
main = do
    input <- getContents
    putStr $ show $ readFrom input

readFrom :: String -> [[String]]
readFrom input = do
    let commands = parseCommands input
    let board = Board.createBoard $ commands !! 0
    board

-- Parses a string with all the commands
-- Returns a 2D list of strings where each sublist is each string in the command
parseCommands :: String -> [[String]]
parseCommands inputString = map (words) $ lines inputString

runCommands commands = do
    Board.createBoard $ commands !! 0

-- just kept as a funtion not a module
-- it "moves l l+ .." -> "ll+.."
moveArray list = do
filter (/=' ') (concat $ tail list)
