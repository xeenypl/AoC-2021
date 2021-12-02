
import Data.List.Split

data Dir = 
      Forward Int
    | Down    Int
    | Up      Int
    deriving (Show)

parse :: String -> [Dir]
parse = map parseLine . lines
    where parseLine :: String -> Dir
          parseLine ln = let split = splitOn " " ln
                             kword = head split
                             value = read $ split !! 1
                         in case kword of 
                            "forward" -> Forward value
                            "down"    -> Down    value
                            "up"      -> Up      value
                            otherwise -> error "invalid file"

eval :: (Int, Int) -> Dir -> (Int, Int)
eval (x, y) (Forward d) = (x + d, y)
eval (x, y) (Down    d) = (x, y + d)
eval (x, y) (Up      d) = (x, y - d)

solve :: String -> Int
solve = uncurry (*) . foldl eval (0, 0) . parse

main :: IO ()
main = readFile "input.txt" >>= print . solve
