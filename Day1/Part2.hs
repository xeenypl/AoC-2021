import Data.List

-- Part II
solve' :: String -> Int
solve' xs = snd $ foldl fn (head ints, 0) $ ints
    where ints = cal_sums $ map read $ lines xs :: [Int]
          fn (last, acc) x | x > last  = (x, acc + 1)
                           | otherwise = (x, acc)
          cal_sums (x1:x2:x3:xs) = 
              (x1 + x2 + x3) : if length xs > 0
                             then cal_sums (x2:x3:xs)
                             else []


main :: IO ()
main = readFile "input.txt" >>= print . solve'
