import Control.Arrow
import Data.Int
import Data.List 
import Data.Maybe
import Data.Char 

main :: IO ()
main = interact $ lines >>> drop 1 >>> head >>> words >>> map read >>> solution >>> output >>> unlines

output :: (Bool, [Int]) -> [String]
output (t, xs) = if t then ["YES", unwords $ map show xs] else ["NO"]

solution :: [Int] -> (Bool, [Int])
solution xs = if (sxs !! 0) >= (sxs !! 1 + sxs !! 2) then (False, []) else (True, (sxs !! 2):(sxs !! 0):(sxs !! 1):(drop 3 sxs))
    where sxs = reverse $ sort xs
          l = length xs

