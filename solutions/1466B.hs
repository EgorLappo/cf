import Control.Arrow
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

-- cover base cases, 
-- then for large lists first reverse the list, and run "dynamic programming" fn, 
-- remember that 1) we always want to increment last element, since at wost it doesn't change anything
--               2) we add 1 to the result to cover that last element, as we only run the fn on the tail
solution :: [Int] -> Int 
solution [_] = 1
solution [_,_] = 2
solution xs = 1 + runDP (head rx + 1) (tail rx) where rx = reverse xs 

-- given a current element `x` and the next one `y`,
--  a) if they are equal, you can't do anything, SKIP
--  b) if they differ by just 1, you canNOT increment `y`, so record it as a new number and continue to next step
--  c) if they differ by more than one, we always wish to increment, since at worst we are at the same "diversity", so count `y` as a new number, and continue to next step with `y+1` (i guess this is the "greedy part") 
runDP :: Int -> [Int] -> Int
runDP x [y] | x == y = 0
            | otherwise = 1
runDP x (y:ys) | x == y = runDP y ys
               | x - y == 1 = 1 + runDP y ys
               | otherwise = 1 + runDP (y+1) ys

-- looking at https://codeforces.com/blog/entry/86126, my solution is different: its O(n) theoretically, i guess, but also very neat)))))))))

-- quick test cases
l1 :: [Int]
l1 = [1,1,3,4,4,5] -- -> 6
l2 :: [Int]
l2 = [1,2,2,2,5,6] -- -> 5
l3 :: [Int]
l3 = [1,1,1,2,2,2] -- -> 3
l4 :: [Int]
l4 = [4,4,5,8]     -- -> 4
l5 :: [Int]
l5 = [2,2,3,4,5]   -- -> 5