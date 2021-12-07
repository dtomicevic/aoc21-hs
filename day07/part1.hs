import Data.List.Split (splitOn)

fuel :: Int -> [Int] -> Int
fuel n = sum . map (abs . (n -))

ternary :: (Int -> Int) -> Int -> Int -> Int
ternary f l r | r - l < 3   = minimum $ map f [l .. r]
              | f m1 > f m2 = ternary f m1 r
              | otherwise   = ternary f l m2
 where
  m1 = l + k
  m2 = r - k
  k  = (r - l) `div` 3

solve :: [Int] -> Int
solve xs = ternary (`fuel` xs) l r
 where
  l = minimum xs
  r = maximum xs

main :: IO ()
main = print . solve . map read . splitOn "," =<< getContents
