import Control.Applicative (liftA2)

toCmd :: String -> (String, Int)
toCmd s = (cmd, read x) where (cmd:x:_) = words s

dir :: (String, Int) -> (Int, Int)
dir ('f':_, x) = (x, 0)
dir ('d':_, x) = (0, x)
dir (_    , x) = (0, -x)

move :: (Int, Int) -> (String, Int) -> (Int, Int)
move l = add l . dir where add (p1, d1) (p2, d2) = (p1 + p2, d1 + d2)

pilot :: [(String, Int)] -> (Int, Int)
pilot = foldl move (0, 0)

main :: IO ()
main = print . liftA2 (*) fst snd . pilot . map toCmd . lines =<< getContents

