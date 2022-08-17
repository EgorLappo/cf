import Control.Arrow
import qualified Data.Set as S
import Data.Int
import Data.List 
import Data.Maybe
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as C

main :: IO ()
main = C.interact $ C.lines >>> drop 1 >>> dropOdd >>> map (C.words >>> map readInt >>> solution >>> show >>> C.pack) >>> C.unlines 
    where readInt = C.readInt >>> fromJust >>> fst

dropOdd :: [a] -> [a]
dropOdd [] = []
dropOdd [x] = []
dropOdd (x:y:xs) = y:(dropOdd xs)

solution :: [Int] -> Int
solution xs | l == 1 = length xs 
            | otherwise = 1
            where l = S.size s
                  s = S.fromList xs

solution' :: [Int] -> Int
solution' xs = length $ collapse xs

-- TIME LIMIT EXCEEDED
-- im at ith element 
-- i start summing a(i) + a(i+1) + a(i+2) until i can't
-- 1) either my current sum from a(i) to a(j) is equal to a(j+1)
-- 2) or my current sum PLUS a(j+1) is equal to a(j+2)
-- in both these cases i want to restart with the next one 

collapse :: [Int] -> [Int]
-- base cases
collapse [] = []
collapse [x] = [x]
collapse [x,y] | x == y = [x,y]
               | otherwise = [x+y]
-- main case
collapse l@(x:y:xs) | all (==x) l = l
                    | x == y = collapse (x:(collapse (y:xs)))
                    | x + y == head xs = collapse (x:(collapse (y:xs)))
                    | otherwise = collapse ((x+y):xs)
