import Control.Applicative (liftA2)
import Data.List (transpose)
import qualified Data.Map as M
import Data.List.Split (splitOn)

type Point = (Int, Int)
type PointMap = M.Map Point Int

points :: Point -> Point -> [Point]
points (a1, b1) (a2, b2) =
  [ (a, b) | a <- [min a1 a2 .. max a1 a2], b <- [min b1 b2 .. max b1 b2] ]

parse :: String -> (Point, Point)
parse = toTupl . map (toTupl . map read) . pars
 where
  pars = map (splitOn ",") . filter (/= "->") . words
  toTupl xs = (head xs, last xs)

isHor :: (Point, Point) -> Bool
isHor = liftA2 (==) (snd . fst) (snd . snd)

isVert :: (Point, Point) -> Bool
isVert = liftA2 (==) (fst . fst) (fst . snd)

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
    .   concatMap (uncurry points)
    .   filter (liftA2 (||) isHor isVert)
    .   map parse
    .   lines
    =<< getContents
