import Data.List
import Data.String
-- just kept as a funtion not a module
-- it "moves l l+ .." -> "ll+.."
moveArray :: [String] -> String
moveArray a =filter (/=' ') (concat $ tail a)
