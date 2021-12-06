import Data.List (sort, group)
import Data.Maybe (fromMaybe)
import Control.Arrow ((&&&))
import Control.Applicative (liftA2)
import qualified Data.Map as M
import Data.List.Split (splitOn)

type FishMap = M.Map Integer Integer

sim :: FishMap -> FishMap
sim = foldl f M.empty . M.toList
 where
  f acc (x, n) | x == 0 = spawn x n $ reset x n acc
               | otherwise = decrease x n acc
  reset x = M.insertWith (+) 6
  spawn x = M.insertWith (+) 8
  decrease x = M.insertWith (+) (x - 1)

counter :: [Integer] -> FishMap
counter = M.fromList . map (head &&& fromIntegral . length) . group . sort

answer :: FishMap -> Integer
answer = sum . map snd . M.toList

main :: IO ()
main =
  print
    .   answer
    .   (!! 256)
    .   iterate sim
    .   counter
    .   map read
    .   splitOn ","
    .   head
    .   lines
    =<< getContents
