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
common =
  boolToCh . (>= 0) . foldl (\c x -> if x == '1' then c + 1 else c - 1) 0

filt :: (String -> Char) -> [String] -> String
filt _ [x] = x
filt f xs  = c : filt f (map tail $ filter ((c ==) . head) xs)
  where c = f $ map head xs

main :: IO ()
main = do
  print
    .   liftA2 (*) (bToInt . filt common) (bToInt . filt (compl . common))
    .   lines
    =<< getContents