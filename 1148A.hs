import Control.Arrow
import Data.Int
import Data.List 
import Data.Maybe
import Data.Char

main :: IO ()
main = interact $ lines >>> head >>> words >>> map read >>> solution >>> show

solution :: [Int] -> Int
solution [a,b,c] = c*2 + c'*2 + a' + b'
    where c' = min a b 
          a' = if a - c' > 0 then 1 else 0
          b' = if b - c' > 0 then 1 else 0
