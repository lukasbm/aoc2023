import Data.Char (isDigit)

main = do
  txt <- readFile "test_part1.txt"
  let content = lines txt
  let cols = length (content !! 0)
  -- print ((content !! 1) !! 3) -- its zero indexed (row 2, col 4)
  -- print (sumGrid content)
  print $ iterateWithIndices content

type Grid = [[Char]]

-- access the grid by index and implicitly pad with '.' when out of bounds.
safeIndex :: Grid -> (Int, Int) -> Char
safeIndex grid (x, y) =
  if x < 0 || y < 0 || x >= length grid || y >= length (grid !! 0)
    then '.'
    else grid !! x !! y

-- check if a character is a symbol
isSymbol :: Char -> Bool
isSymbol c = not (isDigit c || c == '.')

-- check if a symbol is around a given index
symbolAround :: Grid -> (Int, Int) -> Bool
symbolAround grid (x, y) =
  -- x = row, y = col
  isSymbol (safeIndex grid (x - 1, y - 1))
    || isSymbol (safeIndex grid (x - 1, y))
    || isSymbol (safeIndex grid (x - 1, y + 1))
    || isSymbol (safeIndex grid (x, y - 1))
    || isSymbol (safeIndex grid (x, y + 1))
    || isSymbol (safeIndex grid (x + 1, y - 1))
    || isSymbol (safeIndex grid (x + 1, y))
    || isSymbol (safeIndex grid (x + 1, y + 1))

iterateWithIndices :: Grid -> [(Int, Int, Char)]
iterateWithIndices arr = [(row, col, safeIndex arr (row, col)) | row <- [0 .. length arr - 1], col <- [0 .. length (arr !! 0) - 1]]

-- iterate over the grid and sum the number if they have a symbol around! (only one of the digits needs to have a symbol)
sumGrid :: Grid -> Int
sumGrid grid = do
  let filtered = foldl go '.' 

  let filtered = filter (\(y, x, elem) -> isDigit elem) (iterateWithIndices grid)
  let numbers = splitWhen
