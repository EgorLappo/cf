import Control.Arrow
import Data.Int
import Data.List 
import Data.Maybe
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as C

main :: IO ()
main = C.interact $ C.lines >>> drop 1 >>> dropOdd >>> map (C.words >>> map readInt >>> solution >>> (\x -> if x then "YES" else "NO") >>> C.pack) >>> C.unlines 
    where readInt = C.readInt >>> fromJust >>> fst

dropOdd :: [a] -> [a]
dropOdd [] = []
dropOdd [x] = []
dropOdd (x:y:xs) = y:(dropOdd xs)

solution :: [Int] -> Bool
solution = sort >>> (zip <*> tail) >>> map (uncurry sub) >>> all (<=1) 
 where sub = flip (-)