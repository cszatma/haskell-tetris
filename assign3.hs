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

readFrom input = do
    --let inputLines = lines input
    --let commands = map (words) inputLines
    --createBoard $ (getCommands input) !! 0
    getCommands input

-- Returns a list of commands
-- Each command is a list of strings that make up the command
getCommands inputString = do
    map (words) $ lines inputString

runCommands commands = do
    Board.createBoard $ commands !! 0

