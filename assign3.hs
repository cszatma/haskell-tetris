import System.IO
import Data.List
import Data.String

words :: String -> [String]
words []  = []
words xxs@(x:xs)
  | x == ' '  = words xs
  | otherwise = ys : words rest
                  where (ys, rest) = break (== ' ') xxs


  formatRow :: Car -> String
  formatRow (a, d:ds, c, x:xs) = a ++ " | " ++ x ++ concat xs ++ " | " ++ show c ++ "
-- cycle list helps u  repeat a list infinitely cycle[list]
