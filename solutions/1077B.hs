import Control.Arrow
import Data.Int
import Data.List 
import Data.Maybe
import Data.Char

main :: IO ()
main = interact $ lines >>> drop 1 >>> head >>> words >>> map read >>> solution >>> show

solution :: [Int] -> Int
solution [] = 0
solution [_] = 0
solution [_,_] = 0
solution (1:0:1:xs) = 1 + solution (0:xs)
solution (x:xs) = solution xs