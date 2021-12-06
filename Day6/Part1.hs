import Data.List.Split

parse :: String -> [Int]
parse = map read . splitOn ","

-- form GHC.Utils.Misc
nTimes :: Int -> (a -> a) -> (a -> a)
nTimes 0 _ = id
nTimes 1 f = f
nTimes n f = f . nTimes (n-1) f


solve = length . nTimes 80 (concat . map fn)
    where fn x | x <=0     = [6,8]
               | otherwise = [x-1]

main :: IO ()
main = readFile "input.txt" >>= print . solve . parse
