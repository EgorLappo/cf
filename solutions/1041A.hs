import Control.Arrow
import Data.Int
import Data.List 
import Data.Maybe
import Data.Char

main :: IO ()
main = interact $ lines >>> drop 1 >>> head >>> words >>> map read >>> solution >>> show

solution :: [Int] -> Int
solution = sort >>> zip <*> tail >>> map (uncurry sub) >>> map (sub 1) >>> sum
    where sub = flip (-)