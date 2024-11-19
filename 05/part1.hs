import Data.Char (isAlpha, isDigit, isLetter)
import Data.List (find)
import Data.Maybe (fromMaybe)

data Range = Range
  { dest :: Int,
    source :: Int,
    length :: Int
  }
  deriving (Show)

data Map = Map String [Range] deriving (Show)

instance Eq Map where
  (Map name1 _) == (Map name2 _) = name1 == name2

-- parse a range line
parseRangeLine :: String -> Range
parseRangeLine line =
  let [dst, src, len] = map (read :: String -> Int) (words line)
   in Range dst src len

-- parse name of a map
parseNameLine :: String -> String
parseNameLine = takeWhile (/= ' ')

-- parse blocks
-- assume that no empty lines are present
parseBlocks :: [String] -> [Map]
parseBlocks [] = []
parseBlocks inp = foldl go [] inp
  where
    go :: [Map] -> String -> [Map]
    go prev line
      | isLetter (head line) = Map (parseNameLine line) [] : prev
      | isDigit (head line) =
          let (Map name ranges) = head prev
           in Map name (parseRangeLine line : ranges) : tail prev
      | otherwise = prev

parseSeedLine :: String -> [Int]
parseSeedLine x = map (read :: String -> Int) $ words $ drop 6 x

main :: IO ()
main = do
  txt <- readFile "test_part1.txt"
  -- remove empty lines
  let nel = filter (/= "") (lines txt)
  -- get seeds
  let seeds = parseSeedLine $ head nel
  -- split blocks
  let blocks = reverse $ parseBlocks (tail nel)
  -- very important that the blocks are in the correct order (is now the case!)

  let locations = map (traverseMaps blocks) seeds

  print $ minimum locations

inRange :: Range -> Int -> Bool
inRange (Range dst src len) val = val >= src && val < src + len

applyRange :: Range -> Int -> Int
applyRange (Range dst src len) val = dst + (val - src)

translate :: Map -> Int -> Int
translate (Map _ ranges) val =
  let filteredRanges = find (`inRange` val) ranges
   in case filteredRanges of
        Just r -> applyRange r val
        Nothing -> val

traverseMaps :: [Map] -> Int -> Int
traverseMaps maps val = foldl go val maps
  where
    go :: Int -> Map -> Int
    go acc m = translate m acc
