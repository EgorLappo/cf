import Control.Arrow
import Data.Function
import Data.Int
import Data.List
import Data.Maybe

main :: IO ()
main = interact $ lines >>> head >>> words >>> map (\x -> read x :: Int) >>> sln >>> show

sln :: [Int] -> Int
sln [n,m,a] = (n `div` a + rem_n) * (m `div` a + rem_m)
    where rem_n = if n `mod` a == 0 then 0 else 1
          rem_m = if m `mod` a == 0 then 0 else 1