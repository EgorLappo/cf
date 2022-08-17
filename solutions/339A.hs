import Control.Arrow
import Data.Int
import Data.List 
import Data.Maybe
import Data.Char 

main :: IO ()
main = interact $ solution

solution :: String -> String
solution = split '+' >>> map (\n -> read n :: Int) >>> sort >>> map show >>> concat >>> intersperse '+' 

split :: Char -> String -> [String]
split c s = foldr (\a b@(x:xs)-> if a == c then ([]:b) else (a:x):xs) [[]] s