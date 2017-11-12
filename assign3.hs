module Main where

-- Import necessary libraries
import System.IO
import Data.List
import Data.String
import Board

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
    createBoard $ commands !! 0
{-
words :: String -> [String]
words []  = []
words xxs@(x:xs)
  | x == ' '  = words xs
  | otherwise = ys : words rest
                  where (ys, rest) = break (== ' ') xxs


formatRow :: Car -> String
  formatRow (a, d:ds, c, x:xs) = a ++ " | " ++ x ++ concat xs ++ " | " ++ show c ++ "
-- cycle list helps u  repeat a list infinitely cycle[list]
-}

