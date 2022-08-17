import Control.Arrow
import Data.Function
import Data.Int
import Data.List
import Data.Maybe
import Data.Char 

main = do 
    s <- getLine
    putStrLn ((toUpper $ head s):(tail s))