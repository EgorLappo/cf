import Control.Arrow
import Data.Int
import Data.List 
import Data.Maybe
import Data.Char 

main :: IO ()
main = interact $ lines >>> drop 1 >>> head >>> words >>> map read >>> solution >>> map show >>> unwords

solution :: [Int] -> [Int] 
solution xs | null oxs = xs
            | null exs = xs
            | otherwise = sxs
    where 
        oxs = filter odd xs
        exs = filter even xs
        sxs = sort xs