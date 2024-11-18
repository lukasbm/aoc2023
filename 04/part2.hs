type Card = ([Int], [Int])

type AheadList = [Int] -- keeps track of the number of cards

parseCard :: String -> Card
parseCard s =
  let content = tail $ dropWhile (/= ':') s
   in ( map (read :: String -> Int) $ words $ takeWhile (/= '|') content,
        map (read :: String -> Int) $ words $ tail $ dropWhile (/= '|') content
      )

main :: IO ()
main = do
  txt <- readFile "input.txt"
  let cards = map parseCard (lines txt)
  let out = foldl elemOp (replicate (length cards) 1) (zip cards [0 ..])
  print $ sum out

addAt :: AheadList -> Int -> [Int] -> AheadList
addAt old n new = do
  let (before, after) = splitAt n old
  let (relevant, rest) = splitAt (length new) after
  before ++ zipWith (+) relevant new ++ rest

-- use winningCards in here to adapt ahead list
elemOp :: AheadList -> (Card, Int) -> AheadList
elemOp acc (c, i) =
  let n = winningCards c
   in addAt acc (i + 1) (replicate n (acc !! i))

-- number of winning cards
winningCards :: Card -> Int
winningCards (myNumbers, winningNumbers) = length $ filter (`elem` winningNumbers) myNumbers
