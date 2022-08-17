import Control.Arrow
import Data.Int
import Data.List 
import Data.Maybe
import Data.Char

main :: IO ()
main = interact $ lines >>> drop 1 >>> map (words >>> map read >>> (\[x,y] -> solution x y) >>> show) >>> unlines

solution :: Int -> Int -> Int
solution x y 
    | x == y = 0
    | x > y = if odd (x-y) then 2 else 1
    | x < y = if odd (x-y) then 1 else 2