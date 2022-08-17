import Control.Arrow
import Data.Function
import Data.Int
import Data.List 
import Data.Maybe
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as C



main :: IO ()
main = C.interact $ C.lines >>> solveH >>> C.pack

solveH :: [C.ByteString] -> String
solveH lines = solve k (C.unpack s) 
    where k = fst $ fromJust $ C.readInt ((C.words $ head lines) !! 1)
          s = lines !! 1

solve :: Int -> String -> String 
solve 0 s = s
solve k s = map fst $ sortBy (compare `on` snd) $ drop k x
    where x = sort $ zip s [1..]


-- chars = "abcdefghijklmkopqrstuvwxyz"

-- solve :: Int -> String -> String -> String
-- solve 0 _ s = s
-- solve _ [] s = s
-- solve n (c:cs) s = case find_elim c s of (True, x)  -> solve (n-1) (c:cs) x
--                                          (False, _) -> solve n cs s

-- find_elim :: Char -> String -> (Bool, String) 
-- find_elim _ [] = (False, [])
-- find_elim c (x:xs) | c == x    = (True, xs)
--                    | otherwise = case find_elim c xs of (True, s) -> (True, x:s)
--                                                         (False, s) -> (False, x:s)
