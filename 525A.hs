import Control.Arrow
import Data.Function
import Data.Int
import Data.List
import Data.Maybe
import Data.Char
import qualified Data.Map as Map

main :: IO ()
main = interact $ lines >>> drop 1 >>> head >>> solution >>> show 

solution :: String -> Int
solution = (Map.foldl' (+) 0) . (Map.map fst) . (mkMap Map.empty)

-- the map is with tuples 
-- first entry is thee running tally that we later sum, the second counts keys that are available

mkMap :: Map.Map Char (Int,Int) -> String -> Map.Map Char (Int,Int)
mkMap m [] = m
mkMap m (x:xs) = mkMap' (Map.insertWith addKey x (0,1) m) xs

mkMap' :: Map.Map Char (Int,Int) -> String -> Map.Map Char (Int,Int)
mkMap' m (x:xs) = mkMap (Map.insertWith addDoor (toLower x) (1,0) m) xs

addKey :: (Int, Int) -> (Int, Int) -> (Int, Int)
addKey _ (x, n) = (x, n+1) -- we get a key but nothing use it on YET

addDoor :: (Int, Int) -> (Int, Int) -> (Int, Int)
addDoor _ (x, 0) = (x+1, 0) -- we get a door and no keys to open
addDoor _ (x, n) = (x, n-1) -- we get a door and use one key

-- afterthought: this is muuch harder than the official solution
-- but i think that it's because they keep a global mutable variable ans, which i can't do here