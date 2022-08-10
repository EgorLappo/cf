import Control.Arrow
import Data.Int
import Data.List 
import Data.Maybe
import Data.Char

main :: IO ()
main = interact $ lines >>> drop 1 >>> map (words >>> map read >>> (\[x,y] -> (x,y))) >>> solution >>> (\x -> if x then "Happy Alex" else "Poor Alex")

solution :: [(Int, Int)] -> Bool
solution = sortOn fst >>> map snd >>> check

check :: [Int] -> Bool
check []  = False
check [_] = False
check (x:y:xs) | x > y = True
               | otherwise = check (y:xs)