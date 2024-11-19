import Data.Char (isAlpha, isDigit, isLetter)
import Data.List (find, minimumBy)

data Range = Range
  { dest :: Int,
    source :: Int,
    length :: Int
  }
  deriving (Show)

instance Eq Range where
  (Range d1 s1 l1) == (Range d2 s2 l2) = d1 == d2 && s1 == s2 && l1 == l2

instance Ord Range where
  (Range d1 s1 l1) <= (Range d2 s2 l2) = d1 <= d2

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

inSeedLine :: [(Int, Int)] -> Int -> Bool
inSeedLine pairs val = any (\(a, b) -> val >= a && val < a + b) pairs

-- can't generate a simple full list of seeds, would be >5B elements
parseSeedLine :: String -> [(Int, Int)]
-- let pairs = takePairs $ map (read :: String -> Int) $ words $ drop 6 x
--  in concatMap (\(a, b) -> [a .. a + b]) pairs
parseSeedLine x = takePairs $ map (read :: String -> Int) $ words $ drop 6 x
  where
    takePairs :: [a] -> [(a, a)]
    takePairs [] = []
    takePairs (x : y : xs) = (x, y) : takePairs xs

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

  print $ traverseMaps blocks 13
  print $ reverseTraverseMaps blocks 35

  -- let locations = map (traverseMaps blocks) seeds
  -- print $ minimum locations

  let location_map = last blocks

  let (Map _ ranges) = location_map
  print $ minimum ranges
  let mr = minimum ranges

  print $ reverseTraverseMaps blocks (applyRange mr (dest mr))

inRange :: Range -> Int -> Bool
inRange (Range dst src len) val = val >= src && val < src + len

inDestRange :: Range -> Int -> Bool
inDestRange (Range dst src len) val = val >= dst && val < dst + len

applyRange :: Range -> Int -> Int
applyRange (Range dst src len) val = dst + (val - src)

applyDestRange :: Range -> Int -> Int
applyDestRange (Range dst src len) val = src + (val - dst)

translate :: Map -> Int -> Int
translate (Map _ ranges) val =
  let r = find (`inRange` val) ranges
   in case r of
        Just r -> applyRange r val
        Nothing -> val

reverseTranslate :: Map -> Int -> Int
reverseTranslate (Map _ ranges) val =
  let r = find (`inDestRange` val) ranges
   in case r of
        Just r -> applyDestRange r val
        Nothing -> val

traverseMaps :: [Map] -> Int -> Int
traverseMaps maps val = foldl go val maps
  where
    go :: Int -> Map -> Int
    go acc m = translate m acc

reverseTraverseMaps :: [Map] -> Int -> Int
reverseTraverseMaps maps val = foldl go val (reverse maps)
  where
    go :: Int -> Map -> Int
    go acc m = reverseTranslate m acc

-- TODO: check if the dest range of the last map also has billions of elements! --> YES IT HAS
