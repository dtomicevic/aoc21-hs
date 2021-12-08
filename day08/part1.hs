import Data.List (sort)

count :: String -> Bool
count xs = l `elem` [2, 3, 4, 7] where l = length xs

main :: IO ()
main =
  print
    . sum
    . map (fromEnum . count)
    . concatMap (drop 11 . words)
    . lines
    =<< getContents
