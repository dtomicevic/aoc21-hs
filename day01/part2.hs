import Control.Applicative (liftA2)

main :: IO ()
main =
  print
    .   sum
    .   map fromEnum
    .   liftA2 (zipWith (>)) (drop 3) id
    .   map (read :: String -> Int)
    .   lines
    =<< getContents