import Control.Arrow
import Data.Function
import Data.Int
import Data.List
import Data.Maybe
import Data.Char

main :: IO () 
main = interact $ lines >>> drop 1 >>> head >>> words >>> (\ls -> if elem "1" ls then "HARD" else "EASY") 