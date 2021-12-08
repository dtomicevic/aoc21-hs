import qualified Data.Map as M
import Control.Applicative (liftA2)
import Data.List (sort, (\\))
import Data.Maybe (fromJust)
import Control.Arrow (first)

type DigitMap = M.Map String Int

is0 :: String -> String -> Bool
is0 _5 xs | length xs /= 6 = False
          | otherwise      = not . null $ _5 \\ xs

is2 :: String -> String -> Bool
is2 _5 xs | length xs /= 5 = False
          | otherwise      = (== 2) . length $ _5 \\ xs

is3 :: String -> String -> Bool
is3 _5 xs | length xs /= 5 = False
          | otherwise      = (== 1) . length $ _5 \\ xs

is5 :: String -> String -> Bool
is5 bd xs | length xs /= 5 = False
          | otherwise      = null $ bd \\ xs

is6 :: String -> String -> Bool
is6 _1 xs | length xs /= 6 = False
          | otherwise      = not . null $ _1 \\ xs

is9 :: String -> String -> Bool
is9 _4 xs | length xs /= 6 = False
          | otherwise      = null $ _4 \\ xs

decode :: [String] -> DigitMap
decode ks = M.fromList $ zip ys [0 ..]
 where
  _0 = f3 $ is0 _5
  _1 = f1 2
  _2 = f3 $ is2 _5
  _3 = f3 $ is3 _5
  _4 = f1 4
  _5 = f3 $ is5 (_4 \\ _1)
  _6 = f3 $ is6 _1
  _7 = f1 3
  _8 = f1 7
  _9 = f3 $ is9 _4
  fn n = filter ((== n) . length) ks
  f1 = head . fn
  f3 f = head $ dropWhile (not . f) ks
  ys = [_0, _1, _2, _3, _4, _5, _6, _7, _8, _9]

solve1 :: ([String], [String]) -> Int
solve1 (ss, xs) = toInt $ map f xs
 where
  ks = decode ss
  f x = fromJust $ M.lookup x ks
  toInt = sum . zipWith (*) [1000, 100, 10, 1]

main :: IO ()
main =
  print
    .   sum
    .   map (solve1 . liftA2 (,) (take 10) (drop 11) . map sort . words)
    .   lines
    =<< getContents
