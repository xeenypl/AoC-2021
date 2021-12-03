import Data.List

fromBin :: [Bool] -> Int
fromBin num = foldr (+) 0 $ map fn $ zip num $ reverse [0..len-1]
    where len = length num
          fn (a, b) = if a then 2 ^ b else 0


solve' :: (Int -> Int -> Bool) -> Int -> [String]  -> String
solve' test n xs = if length xs == 1 then head xs else
    if length (filter (=='0') row) `test` (length row `div` 2)
    then solve' test (n + 1) $ filter (\x -> (x !! n) == '0') xs
    else solve' test (n + 1) $ filter (\x -> (x !! n) == '1') xs
    where row = transpose xs !! n


solve :: String -> Int
solve src = let get f = fromBin . map (=='1') . solve' f 0 . lines
                o     = get (>)  src
                co2   = get (<=) src
            in o * co2

main :: IO ()
main = readFile "input.txt" >>= print . solve
