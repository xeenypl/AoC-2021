import Data.List.Extra

parse :: String -> [((Int, Int), (Int, Int))]
parse = map (toPiar . map (toPiar . map read . splitOn ",") . splitOn " -> ") . lines
    where toPair [x,y] = (x,y)

pathToPoint :: ((Int, Int), (Int, Int)) -> [(Int, Int)]
pathToPoint ((ax, ay), (bx, by)) 
        | ax == bx  = [(ax, y) | y <- [top ..bot]]
        | ay == by  = [(x, ay) | x <- [left..right]]
        | ay <  by && ax < bx || ay > by && ax > bx
                    = [(left  + x, top + x) | x <- [0..bot-top]]
        | otherwise = [(right - x, top + x) | x <- [0..bot-top]]
    where top   = min ay by
          bot   = max ay by
          left  = min ax bx
          right = max ax bx

counts :: (Eq a, Ord a) => [a] -> [(a,Int)]
counts = foldl counts' [] . sort
    where counts' :: Eq a => [(a, Int)] -> a -> [(a, Int)]
          counts' []         x             = [(x, 1)]
          counts' ((a,c):as) x | a == x    = (a,c+1):as
                               | otherwise = (x,1):(a,c):as

solve :: String -> Int
solve = length . filter (>1) . map snd . counts . concat . map pathToPoint . parse

main :: IO ()
main = readFile "input.txt" >>= print . solve
