import Control.Arrow
import Data.Function
import Data.Int
import Data.List
import Data.Maybe
import Data.Char

main :: IO ()
main = interact $ lines >>> drop 1 >>> map (words >>> map read >>> solution >>> (\b -> if b then "Yes" else "No")) >>> unlines

solution :: [Int] -> Bool
solution [n,a,b,c,d] = if ((a-b)*n <= c+d) && ((a+b)*n >= c-d) then True else False