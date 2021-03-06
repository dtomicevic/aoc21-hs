import Control.Applicative (liftA2)
import Data.List (transpose)
import Data.Maybe (listToMaybe, isNothing, fromJust)
import Control.Arrow (second)
import Data.List.Split (splitOn, chunksOf)

isWin5 :: [Int] -> Bool
isWin5 = all (< 0)

isWin :: [[Int]] -> Bool
isWin = liftA2 (||) (any isWin5) (any isWin5 . transpose)

mark :: Int -> [[[Int]]] -> [[[Int]]]
mark n = map (map (map (\x -> if x == n then -1 else x)))

getWin :: [[[Int]]] -> Maybe [[Int]]
getWin = listToMaybe . dropWhile (not . isWin)

score :: Int -> [[Int]] -> Int
score n = (n *) . sum . filter (>= 0) . concat

main :: IO ()
main = do
  ss <- filter (/= "") . lines <$> getContents
  let xs = map read . splitOn "," . head $ ss :: [Int]
  let bs = chunksOf 5 . map (map read . words) . tail $ ss :: [[[Int]]]

  let win =
        uncurry score
          . second fromJust
          . head
          . dropWhile (isNothing . snd)
          . zipWith (curry (second getWin)) xs
          . tail
          $ scanl (flip mark) bs xs

  print win
