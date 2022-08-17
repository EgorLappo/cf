import Control.Arrow
import Data.Function
import Data.Int
import Data.List 
import Data.Maybe
import Data.Char

main :: IO ()
main = interact $ words >>> map (\n -> read n :: Int) >>> solveH >>> show

solveH :: [Int] -> Int
solveH [n, m, a, b] = solve n m a b

solve :: Int -> Int -> Int -> Int -> Int
solve 0 _ _ _ = 0
solve n m a b 
  | m*a <= b  = n*a -- нет смысла покупать абонемент
  | otherwise = if m <= n then (b + solve (n-m) m a b) else -- можно купить абонемент
                    if n*a <= b then n*a else b