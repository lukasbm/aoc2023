type Card = ([Int], [Int])

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
  let scores = map (calcScore . winningCards) cards
  print $ sum scores

calcScore :: Int -> Int
calcScore 0 = 0
calcScore n = 2 ^ (n - 1)

-- number of winning cards
winningCards :: Card -> Int
winningCards (myNumbers, winningNumbers) = length $ filter (`elem` winningNumbers) myNumbers
