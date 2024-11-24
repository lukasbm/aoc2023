import Data.Char (isAlphaNum, isLetter)
import Data.List (find, minimumBy)
import Debug.Trace

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
  txt <- readFile "test_part2.txt"

  -- define an infinite stream of RL  ... use cycle!
  let directions = cycle $ head $ lines txt

  let graph = map (parseNode . filter isAlphaNum) (drop 2 $ lines txt) :: [Node]

  let starterNodes = filter (\n -> last (name n) == 'A') graph

  print $ take 3 $ walkAll graph directions starterNodes

walkAll :: [Node] -> [Char] -> [Node] -> [Node]
walkAll graph (d : ds) nodes = map (\n -> walk graph (d : ds) n) nodes

-- walks infinitely (given directions is infinite)
-- graph, directions, current node, step counter
walk :: [Node] -> [Char] -> Node -> [Node]
walk graph (d : ds) curr
  | d == 'R' = curr : walk graph ds (findNode graph (right curr))
  | d == 'L' = curr : walk graph ds (findNode graph (left curr))
  | otherwise = error "solve: invalid direction"
