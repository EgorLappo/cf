import Control.Arrow
import Data.Int
import Data.List 
import Data.Maybe
import Data.Char ()

main :: IO ()
main = interact $ lines >>> drop 1 >>> head >>> words >>> map read >>> solution >>> show

solution :: [Int] -> Int
solution xs = (sort xs) !! n
    where l = length xs
          n = if even l then l `div` 2 -1 else l `div` 2