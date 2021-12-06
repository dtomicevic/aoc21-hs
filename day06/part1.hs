import Data.List.Split (splitOn, chunksOf)

sim :: [Int] -> [Int]
sim xs = xs' ++ replicate n 8
 where
  xs' = map (\x -> if x == 0 then 6 else x - 1) xs
  n   = length $ filter (== 0) xs

main :: IO ()
main =
  print
    .   length
    .   (!! 80)
    .   iterate sim
    .   map read
    .   splitOn ","
    .   head
    .   lines
    =<< getContents
