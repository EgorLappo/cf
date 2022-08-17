import Control.Arrow
import Data.Function
import Data.Int
import Data.List 
import Data.Maybe
import Data.Char

answers = ['A', 'B', 'C', 'D', 'E']

main :: IO ()
main = interact $ lines >>> drop 1 >>> inputHelper >>> show 

inputHelper :: [String] -> Int
inputHelper lns = solve answers points 
    where answers = init lns
          points = map (\n -> read n :: Int) $ words $ last lns

solve :: [String] -> [Int] -> Int
solve answers points = sum $ map (uncurry (*)) $ zip points maxAnswers
    where maxAnswers = getMaxAnswers $ transpose answers

getMaxAnswers :: [String] -> [Int]
getMaxAnswers [] = []
getMaxAnswers (x:xs) = (maximum [length $ filter (== ans) x | ans <- answers]):(getMaxAnswers xs)