import Data.List (sort, sortBy)
import Data.Ord (comparing)

--------- data structures

data Combo = NoCombo | HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind deriving (Show, Eq, Ord, Enum)

data Card = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Show, Eq, Ord, Enum)

type Hand = (Card, Card, Card, Card, Card)

--------- parsing

parseCard :: Char -> Card
parseCard '2' = Two
parseCard '3' = Three
parseCard '4' = Four
parseCard '5' = Five
parseCard '6' = Six
parseCard '7' = Seven
parseCard '8' = Eight
parseCard '9' = Nine
parseCard 'T' = Ten
parseCard 'J' = Jack
parseCard 'Q' = Queen
parseCard 'K' = King
parseCard 'A' = Ace
parseCard _ = error "Invalid card"

parseHand :: [Char] -> Hand
parseHand (a : b : c : d : e : _) = (parseCard a, parseCard b, parseCard c, parseCard d, parseCard e)

main :: IO ()
main = do
  txt <- readFile "test_part1.txt"

  let hands :: [Hand] = map (parseHand . take 5) (lines txt)
  let bids :: [Int] = map ((read :: String -> Int) . drop 6) (lines txt)

  print $ fromEnum TwoPair
  print $ fromEnum Three

  -- solution
  print $ bids
  print $ rankHands hands

  -- FIXME: need to sort bids together with the hand ranks!!!!

  let winnings = zipWith (+) (rankHands hands) bids
  print $ sum winnings

--------- solution

-- Helper Function to get sorting indices
-- NOTE: 1 based index
sortingIndices :: (Ord a) => [a] -> [Int]
sortingIndices xs = map snd $ sortBy (comparing fst) $ zip xs [1 ..]

-- sort the hands based on the combo and the card values
rankHands :: [Hand] -> [Int]
rankHands hands = sortingIndices $ map handScore hands

-- TODO: work here!
determineCombo :: Hand -> Combo
determineCombo (a, b, c, d, e) =
  let sortedCards = sort [a, b, c, d, e]
   in case sortedCards of
        [s1, s2, s3, s4, s5] -> determineComboFromSorted (s1, s2, s3, s4, s5)

determineComboFromSorted :: Hand -> Combo
determineComboFromSorted (a, b, c, d, e)
  | a == b && b == c && c == d && d == e = FiveOfAKind
  | (a == b && b == c && c == d) || (b == c && c == d && d == e) = FourOfAKind
  | (a == b && b == c && d == e) || (a == b && c == d && d == e) = FullHouse
  | (a == b && b == c) && (d == e) || (a == b) && (c == d && d == e) = ThreeOfAKind
  | (a == b && c == d) || (a == b && d == e) || (b == c && d == e) = TwoPair
  | a == b || b == c || c == d || d == e = OnePair
  | otherwise = HighCard

-- a combo is always strong than any lower combo with higher card values
-- 100 * combo + card
-- NOTE: card value is not only determined by the first card, but by the whole hand
-- need to do pairwise comparison, then sort the list
handScore :: Hand -> Int
handScore hand@(a, b, c, d, e) = 100000 * fromEnum (determineCombo hand) + 10000 * fromEnum a + 1000 * fromEnum b + 100 * fromEnum c + 10 * fromEnum d + fromEnum e
