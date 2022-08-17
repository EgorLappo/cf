import Control.Arrow
import Data.Function
import Data.Int
import Data.List
import Data.Maybe

main = interact $ lines >>> map (\l -> map (\n -> read n :: Int) $ words l) >>> getPos >>> soln >>> show 

getPos :: [[Int]] -> (Int, Int)
getPos m = getPos' m 0 
     where getPos'(x:xs) k = if (elem 1) x then (k-2, head ([i | (e, i) <- zip x [0..], e == 1])-2) else getPos' xs (k+1)

soln (x,y) = abs x + abs y