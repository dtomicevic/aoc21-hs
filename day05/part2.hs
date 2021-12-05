import Control.Applicative (liftA2)
import Data.List (transpose)
import qualified Data.Map as M
import Data.List.Split (splitOn)

type Point = (Int, Int)
type PointMap = M.Map Point Int

list :: Int -> Int -> [Int]
list a b | a > b     = reverse $ list b a
         | otherwise = [a .. b]

points :: (Point, Point) -> [Point]
points l@((x1, y1), (x2, y2))
  | isHorVert l
  = [ (x, y) | x <- [min x1 x2 .. max x1 x2], y <- [min y1 y2 .. max y1 y2] ]
  | otherwise
  = zip (list x1 x2) (list y1 y2)

parse :: String -> (Point, Point)
parse = toTupl . map (toTupl . map read) . pars
 where
  pars = map (splitOn ",") . filter (/= "->") . words
  toTupl xs = (head xs, last xs)

isHor :: (Point, Point) -> Bool
isHor = liftA2 (==) (snd . fst) (snd . snd)

isVert :: (Point, Point) -> Bool
isVert = liftA2 (==) (fst . fst) (fst . snd)

isHorVert :: (Point, Point) -> Bool
isHorVert = liftA2 (||) isHor isVert

ins :: Point -> PointMap -> PointMap
ins p = M.insertWith (\a _ -> a + 1) p 1

main :: IO ()
main = do
  print
    .   length
    .   filter (> 1)
    .   map snd
    .   M.toList
    .   foldl (flip ins) M.empty
    .   concatMap (points . parse)
    .   lines
    =<< getContents
