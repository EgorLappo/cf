import Control.Arrow
import Data.Int
import Data.List 
import Data.Maybe
import Data.Char

main :: IO ()
main = interact $ lines >>> drop 1 >>> map (read >>> solution) >>> unlines

solution :: Int -> String
solution 3 = "7"
solution n | odd n  = '7':(solution (n-3))
           | even n = take (n `div` 2) $ repeat '1'