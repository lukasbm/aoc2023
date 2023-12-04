import Data.Char (isDigit)

_split :: Char -> String -> [String]
_split _ "" = []
_split c s = firstWord : _split c rest
  where
    firstWord = takeWhile (/= c) s
    rest = drop (length firstWord + 1) s

_removeChar :: Char -> String -> String
_removeChar _ [] = []
_removeChar ch (c : cs)
  | c == ch = _removeChar ch cs
  | otherwise = c : _removeChar ch cs

_prefixOf :: String -> String -> Bool
_prefixOf "" _ = True
_prefixOf _ "" = False
_prefixOf prefix str =
  if head prefix == head str
    then _prefixOf (tail prefix) (tail str)
    else False

_suffixOf :: String -> String -> Bool
_suffixOf suffix str = _prefixOf (reverse suffix) (reverse str)

_reverse :: String -> String
_reverse "" = ""
_reverse (c : cs) = _reverse cs ++ [c]

data Game = Game
  { _id :: Int,
    draws :: [Draw]
  }
  deriving (Show)

data Draw = Draw
  { red :: Maybe Int,
    green :: Maybe Int,
    blue :: Maybe Int
  }
  deriving (Show)

main = do
  txt <- readFile "test.txt"
  let content = lines txt
  let games = map parseGame content
  print games

parseGame :: String -> Game
parseGame line = do
  let splitLine = _split ':' line
  let gameId = read (filter isDigit (head splitLine))
  let draws = map parseDraw (_split ';' (last splitLine))
  Game {_id = gameId, draws = draws}

parseDraw :: String -> Draw
parseDraw drawLine =
  let drawSplit = _split ',' drawLine
   in Draw
        { red = findColor drawSplit "red",
          green = findColor drawSplit "green",
          blue = findColor drawSplit "blue"
        }

findColor :: [String] -> String -> Maybe Int
findColor [] _ = Nothing
findColor colorLine color
  | color `_suffixOf` head colorLine = Just (read (filter isDigit (head colorLine)))
  | otherwise = findColor (tail colorLine) color
