import Control.Arrow
import Data.Function
import Data.Int
import Data.List 
import Data.Maybe
import Data.Char

chars = "abcdefghijklmkopqrstuvwxyz"

main :: IO ()
main = do
    line <- getLine 
    let k = read ((words line) !! 1) :: Int
    s <- getLine
    putStrLn $ solve k chars s

solve :: Int -> String -> String -> String
solve 0 _ s = s
solve _ [] s = s
solve n (c:cs) s | elem c s  = solve (n-1) (c:cs) $ elim c s
                 | otherwise = solve n cs s

elim :: Char -> String -> String
elim _ [] = []
elim c (x:xs) | x == c    = xs
              | otherwise = x:(elim c xs)