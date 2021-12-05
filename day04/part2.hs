import Control.Applicative (liftA2)
import Data.List (transpose)
import Data.Maybe (listToMaybe, isNothing, fromJust)
import Control.Arrow (second)
-- import Data.List.Split (splitOn, chunksOf)

-- don't know how to import a lib into a single file
-- import Data.List.Split (splitOn, chunksOf)
splitOn :: String -> String -> [String]
splitOn c s = case dropWhile (== head c) s of
  "" -> []
  s' -> w : splitOn c s'' where (w, s'') = break (== head c) s'

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n l | n > 0     = take n l : chunksOf n (drop n l)
             | otherwise = error "Negative or zero n"

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

  let moves = zipWith (,) xs
          . tail
          $ scanl (flip mark) bs xs

  -- let win =
  --       uncurry score
  --         . second fromJust
  --         . head
  --         . dropWhile (isNothing . snd)
  --         $ moves


  print moves
