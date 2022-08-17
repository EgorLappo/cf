import Control.Arrow
import Data.Int
import Data.List 
import Data.Maybe
import Data.Char

main :: IO ()
main = interact $ lines >>> drop 1 >>> head >>> words >>> map read >>> solution >>> (\b -> if b then "YES" else "NO")

solution :: [Int] -> Bool
solution []    = False
solution [_]   = False
solution [_,_] = False
solution xs = tri $ sort xs

tri :: [Int] -> Bool
tri [_] = False
tri [_,_] = False
tri (x:y:xs) = if findGT (x+y) xs then True else tri (y:xs)

findGT :: Int -> [Int] -> Bool
findGT n [] = False
findGT n (x:xs) | n > x = True
                | otherwise = findGT n xs

