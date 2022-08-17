import Control.Arrow
import Data.Function
import Data.Int
import Data.List
import Data.Maybe

main :: IO ()
main = interact $ lines >>> drop 1 >>> (map (\l -> if (elem '+' l) then 1 else (-1))) >>> sum >>> show
