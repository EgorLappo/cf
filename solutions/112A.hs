import Control.Arrow
import Data.Function
import Data.Int
import Data.List
import Data.Maybe
import Data.Char 

main :: IO ()
main = do 
    s1 <- getLine
    s2 <- getLine
    putStrLn . show $ cmp s1 s2

cmp :: String -> String -> Int
cmp [] [] = 0
cmp (s:ss) (t:ts) | s' > t' = 1
                  | s' < t' = -1
                  | otherwise = cmp ss ts
    where s' = toLower s
          t' = toLower t
