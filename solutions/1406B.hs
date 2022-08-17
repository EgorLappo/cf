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

-- just select (at most) top ten candidates from both ends, and bruteforce all combinations, of which we have <= than (10 choose 5) = 252   
solution :: [Int] -> Int
solution xs = findMaxProd mxs
    where mxs = min_sxs ++ max_sxs
          min_sxs = take 5 sxs
          max_sxs = take (min 5 (length sxs - 5)) $ reverse sxs
          sxs = sort xs

findMaxProd :: [Int] -> Int
findMaxProd = combinations 5 >>> map product >>>maximum

-- my solution from 99 Haskell problems p 26
combinations :: Int -> [a] -> [[a]]
combinations _ [] = []
combinations 0 _ = []
combinations 1 xs = fmap (\x -> [x]) xs
combinations n l 
 | n > length l = []
 | otherwise    = concatMap recurseComb lists 
        where lists = take (length l) $ iterate (drop 1) l
              recurseComb xs = fmap ((:) (head xs)) (combinations (n-1) (tail xs))