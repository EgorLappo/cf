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
solution xs = undefined
    where sxs = sort xs
          l = length xs

