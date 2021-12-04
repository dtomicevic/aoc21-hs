import Control.Applicative (liftA2)
type Aim = Int
type Pos = Int
type Depth = Int

toCmd :: String -> (String, Int)
toCmd s = (cmd, read x) where (cmd:x:_) = words s

move :: (Aim, (Pos, Depth)) -> (String, Int) -> (Aim, (Pos, Depth))
move (a, (p, d)) ('f':_, x) = (a, (p + x, d + a * x))
move (a, pd    ) ('d':_, x) = (a + x, pd)
move (a, pd    ) (_    , x) = (a - x, pd)

pilot :: [(String, Int)] -> (Pos, Depth)
pilot = snd . foldl move (0, (0, 0))

main :: IO ()
main = print . liftA2 (*) fst snd . pilot . map toCmd . lines =<< getContents
