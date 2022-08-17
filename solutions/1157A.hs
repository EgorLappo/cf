import Control.Arrow
import Data.Function
import Data.Int
import Data.List 
import Data.Maybe
import Data.Char

main :: IO ()
main = do 
    n <- getLine
    putStrLn $ show $ solve (read n :: Int)

solve :: Int -> Int 
solve n | n <= 9 = 9
        | otherwise = if (n+1) `mod` 10 == 0 then 1 + (solve . divideTen) (n+1) else 1 + solve (n + 1)

divideTen :: Int -> Int 
divideTen n | (n `mod` 10) /= 0 = n
            | otherwise = divideTen (n `div` 10)