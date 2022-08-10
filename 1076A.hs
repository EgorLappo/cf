import Control.Arrow
import Data.Int
import Data.List 
import Data.Maybe
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Set as S


main :: IO ()
main = C.interact $ C.lines >>> drop 1 >>> head >>> C.unpack >>> solution >>> C.pack

-- correct solution is to drop the *first* character that is greater than the next one

solution :: String -> String
solution [x,y] | x <= y = [x]
               | x > y = [y]
solution (x:y:xs) | x <= y = x : solution (y:xs)
                  | x > y  = (y:xs)

-- wrong solution of just dropping maximal char that i can find:

-- solution :: String -> String
-- solution s = dropOneChar z s where z = findMaxChar (head s) s

-- findMaxChar :: Char -> String -> Char
-- findMaxChar z [] = z
-- findMaxChar z (x:xs) | z > x = findMaxChar z xs
--                    | otherwise = findMaxChar x xs

-- dropOneChar :: Char -> String -> String
-- dropOneChar z [] = []
-- dropOneChar z (x:xs) | z == x = xs 
--                      | otherwise = x:(dropOneChar z xs)
