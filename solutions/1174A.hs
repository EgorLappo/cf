import Control.Arrow
import Data.Int
import Data.List 
import Data.Maybe
import Data.Char 

main :: IO ()
main = interact $ lines >>> drop 1 >>> head >>> words >>> map read >>> solution >>> map show >>> unwords

solution :: [Int] -> [Int]
solution (x:xs) = if all ((==) x) xs then [-1] else sort (x:xs)