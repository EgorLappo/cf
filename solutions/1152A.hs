import Control.Arrow
import Data.Int
import Data.List 
import Data.Maybe
import Data.Char

main :: IO ()
main = interact $ lines >>> drop 1 >>> map (words >>> map read) >>> solution >>> show

solution :: [[Int]] -> Int
solution [cs, ks] = (min co ke) + (min ce ko)
    where co = length $ filter odd cs
          ce = length $ filter even cs
          ko = length $ filter odd ks
          ke = length $ filter even ks