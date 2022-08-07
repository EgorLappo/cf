import Control.Arrow
import Data.Function
import Data.Int
import Data.List
import Data.Maybe
import Data.Char

main :: IO ()
main = interact $ words >>> map (\n -> read n :: Int) >>> (\l -> soln (l !! 0) (l !! 1)) >>> show

soln :: Int -> Int -> Int
soln x 0 = x
soln x n | x `mod` 10 == 0 = soln (x `div` 10) (n-1)
         | otherwise = soln (x-1) (n-1)