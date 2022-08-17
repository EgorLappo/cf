import Control.Arrow
import Data.Int
import Data.List 
import Data.Maybe
import Data.Char

main :: IO ()
main = interact $ lines >>> head >>> map digitToInt >>> solution >>> map show >>> concat

solution :: [Int] -> [Int]
solution (x:xs) | x == 9 = 9:(map invert xs)
                | otherwise = map invert (x:xs)

invert :: Int -> Int
invert n | n <= 4 = n
         | otherwise = 9 - n