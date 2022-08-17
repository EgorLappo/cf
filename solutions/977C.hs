import Control.Arrow
import Data.Int
import Data.List 
import Data.Maybe
import Data.Char 

main :: IO ()
main = interact $ lines >>> solutionH >>> show 

solutionH :: [String] -> Int
solutionH [l, xs] = solution (read $ words l !! 1) (sort $ map read $ words xs)

solution :: Int -> [Int] -> Int
solution _ [] = (-1)
solution 0 (x:xs) | x == 1 = (-1)
                 | otherwise = x-1
solution 1 [x] = x
solution 1 (x:y:xs) | x == y = (-1)
                    | otherwise = x
solution n xs = solution 1 (drop (n-1) xs)