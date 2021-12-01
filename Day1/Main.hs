import Data.List

--- 1709 
solve :: String -> Int
solve xs = snd $ foldl fn (head ints, 0) $ ints
    where ints = map read $ lines xs :: [Int]
          fn (last, acc) x | x > last  = (x, acc + 1)
                           | otherwise = (x, acc)

main :: IO ()
main = readFile "input.txt" >>= print . solve
