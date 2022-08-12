import Control.Arrow
import Data.Int
import Data.List 
import Data.Maybe
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as C

main :: IO ()
main = C.interact $ C.lines >>> drop 1 >>> grp >>> map (solution >>> C.pack) >>> C.unlines 

grp :: [a] -> [[a]]
grp [] = []
grp [x] = []
grp [x,y] = []
grp (x:y:z:xs) = ([y,z]):(grp xs)

solution :: [C.ByteString] -> String
solution [s, t] = if C.any (=='o') t then "YES" else "NO"