import Data.List

fromBin :: [Bool] -> Int
fromBin num = foldr (+) 0 $ map fn $ zip num $ reverse [0..len-1]
    where len = length num
          fn (a, b) = if a then 2 ^ b else 0

solve :: String -> Int
solve src = gamma * epsilon
    where parseLines = map parseLine . transpose . lines
          parseLine line =      -- Maybe >=
              length (filter (=='0') line) > length line `div` 2
          gamma   = fromBin $ map (not) $ parseLines src
          epsilon = fromBin $             parseLines src 

main :: IO ()
main = readFile "input.txt" >>= print . solve
