
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

eval :: (Int, Int, Int) -> Dir -> (Int, Int, Int)
eval (x, y, aim) (Forward d) = (x + d, y + (d * aim), aim)
eval (x, y, aim) (Down    d) = (x,     y,             aim + d)
eval (x, y, aim) (Up      d) = (x,     y,             aim - d)

solve :: String -> Int
solve = uncurry (*) . tripleToPair . foldl eval (0, 0, 0) . parse
    where tripleToPair (a, b, _) = (a, b)

main :: IO ()
main = readFile "input.txt" >>= print . solve
