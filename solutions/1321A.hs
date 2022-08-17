import Control.Arrow
import Data.Int
import Data.List 
import Data.Maybe
import Data.Char

main = interact $ lines >>> drop 1 >>> map (words >>> map read) >>> solution >>> show

solution :: [[Int]] -> Int
solution [xs,ys] | length f == 0 = -1
                 | otherwise = if npos == 0 then -1 else 1 + ceilIntDiv compensation npos
    where f = filter (uncurry (/=)) $ zip xs ys 
          npos = length $ filter (uncurry (>)) f
          nneg = length $ filter (uncurry (<)) f
          compensation = 1 - (npos - nneg)  -- in the parens is the number of questions by which the second outweighs the first, so we need to compensate by this amount at least plus extra point to win

ceilIntDiv :: Int -> Int -> Int
ceilIntDiv m n = (m `div` n) + r
    where r = if m `mod` n == 0 then 0 else 1