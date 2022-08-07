import Control.Arrow
import Data.Function
import Data.Int
import Data.List
import Data.Maybe
import Data.Char 

main :: IO ()
main = do
    s <- getLine
    putStrLn $ soln s

soln :: [Char] -> String
soln s | l == 4 = "YES"
       | l == 7 = "YES"
       | otherwise = "NO"
    where l = length $ filter (\c -> (c=='4') || (c=='7')) s