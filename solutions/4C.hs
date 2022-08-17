import Control.Arrow
import Data.Function
import Data.Int
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy.Char8 as C

-- LAZY STRING SOLUTION THAT IS NOT WORKING WITH CF JUDGE SYSTEM

main :: IO ()
main = C.interact $ C.lines >>> drop 1 >>> solution >>> C.unlines 

solution :: [C.ByteString] -> [C.ByteString]
solution xs = reg Map.empty xs

reg :: (Map.Map C.ByteString Int) -> [C.ByteString] -> [C.ByteString]
reg m [] = []
reg m (x:xs) = case (Map.lookup x m) of Nothing -> (C.pack "OK") : reg (Map.insert x 1 m) xs
                                        Just n  -> (C.append x (C.pack $ show n)) : reg (Map.insertWith (+) x 1 m) xs

-- REGULAR STRING SOLUTION FOR CODEFORCES

-- main :: IO ()
-- main = interact $ lines >>> drop 1 >>> solution >>> unlines 

-- solution :: [String] -> [String]
-- solution xs = reg Map.empty xs

-- reg :: (Map.Map String Int) -> [String] -> [String]
-- reg m [] = []
-- reg m (x:xs) = case (Map.lookup x m) of Nothing -> "OK" : reg (Map.insert x 1 m) xs
--                                         Just n  -> (x++(show n)) : reg (Map.insertWith (+) x 1 m) xs
