import Control.Arrow
import Data.Int
import Data.List 
import Data.Maybe
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Set as S


main :: IO ()
main = C.interact $ C.lines >>> drop 1 >>> map (C.words >>> map readInt) >>> solutionH >>> map (show >>> C.pack) >>> C.unlines 
    where readInt = C.readInt >>> fromJust >>> fst

solutionH :: [[Int]] -> [Int]
solutionH [] = []
solutionH (x:y:xs) = (solution (head x) y):(solutionH xs)

solution :: Int -> [Int] -> Int
solution n = S.fromList >>> S.size