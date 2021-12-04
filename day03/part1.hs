import Data.List(transpose)
import Control.Applicative (liftA2)

bToInt :: String -> Int
bToInt = sum . zipWith (*) (map (2 ^) [0 ..]) . map (read . pure) . reverse

compl :: Char -> Char
compl '1' = '0'
compl _   = '1'

boolToCh :: Bool -> Char
boolToCh True = '1'
boolToCh _    = '0'

common :: String -> Char
common = boolToCh . (> 0) . foldl (\c x -> if x == '1' then c + 1 else c - 1) 0

main :: IO ()
main = do
  print
    .   liftA2 (*) (bToInt . map common) (bToInt . map (common . map compl))
    .   transpose
    .   lines
    =<< getContents