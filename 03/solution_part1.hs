main = do
  txt <- readFile "test_part1.txt"
  let content = lines txt
  print content

-- TODO: parse into array of [Char], then create 2D datatype from it

-- Have to analyze this space around each number

-- ******
-- *1234*
-- ******

-- Note: numbers can share symbols

-- This might be the first time I have to use a 2D array
-- https://www.reddit.com/r/haskell/comments/loj3x7/2dimensional_algebraic_data_type/

-- solution could be a backtracking algorithm that walks into every direction.
-- start it from every digit in the input
-- will find duplicates, but can just go distinct + sum
-- or use backtracking to get the right sub-rectangle
-- sub rectangle would make sense, also because of behaviour on the edges
-- just need to know the number and then check if symbol in there