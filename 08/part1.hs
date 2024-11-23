import Data.Char (isLetter)
import Data.List (find, minimumBy)

data Node = Node {name :: String, left :: String, right :: String} deriving (Show, Eq)

parseNode :: String -> Node
parseNode [a, b, c, d, e, f, g, h, i] = Node [a, b, c] [d, e, f] [g, h, i]
parseNode _ = error "chunksOfThree: list length not 9"

findNode :: [Node] -> String -> Node
findNode [] _ = error "findNode: node not found"
findNode (x : xs) n
  | n == name x = x
  | otherwise = findNode xs n

main = do
  txt <- readFile "input.txt"

  -- define an infinite stream of RL  ... use cycle!
  let directions = cycle $ head $ lines txt

  let graph = map (parseNode . filter isLetter) (drop 2 $ lines txt) :: [Node]

  print $ solve graph directions (findNode graph "AAA")

-- graph, directions, current node, step counter
solve :: [Node] -> [Char] -> Node -> Int
solve graph (d : ds) curr
  | name curr == "ZZZ" = 0
  | d == 'R' = 1 + solve graph ds (findNode graph (right curr))
  | d == 'L' = 1 + solve graph ds (findNode graph (left curr))
  | otherwise = error "solve: invalid direction"
