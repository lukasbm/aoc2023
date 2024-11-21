import Data.List (sort)

--------- data structures

data Combo = NoCombo | HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind deriving (Show, Eq, Ord)

data Card = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Show, Eq, Ord)

newtype Hand = Hand (Card, Card, Card, Card, Card) deriving (Show)

-- order based on the card value
instance Ord Hand where
  compare :: Hand -> Hand -> Ordering
  compare (Hand (a1, a2, a3, a4, a5)) (Hand (b1, b2, b3, b4, b5)) = do
    let scoreHand1 = 10000 * fromEnum a1 + 1000 * fromEnum a2 + 100 * fromEnum a3 + 10 * fromEnum a4 + fromEnum a5
    let scoreHand2 = 10000 * fromEnum b1 + 1000 * fromEnum b2 + 100 * fromEnum b3 + 10 * fromEnum b4 + fromEnum b5
    compare scoreHand1 scoreHand2

instance Eq Hand where
  (==) :: Hand -> Hand -> Bool
  (Hand (a1, a2, a3, a4, a5)) == (Hand (b1, b2, b3, b4, b5)) = a1 == b1 && a2 == b2 && a3 == b3 && a4 == b4 && a5 == b5

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
parseHand (a : b : c : d : e : _) = Hand (parseCard a, parseCard b, parseCard c, parseCard d, parseCard e)

main :: IO ()
main = do
  txt <- readFile "test_part1.txt"

  let hands :: [Hand] = map (parseHand . take 5) (lines txt)
  let bids :: [Int] = map ((read :: String -> Int) . drop 6) (lines txt)

  print "hi"

  let h1 = Hand (Ace, King, Queen, Jack, Ten)
  let h2 = Hand (Ace, King, Queen, Jack, Nine)
  let h3 = Hand (Two, Four, Three, Three, Three)

  -- print $ h1 == h2
  -- print $ h1 < h2

  print $ sort h3

--------- solution

determineCombo :: Hand -> Combo
determineCombo
  | isFiveOfAKind = FiveOfAKind
  | isFourOfAKind = FourOfAKind
  | isFullHouse = FullHouse
  | isThreeOfAKind = ThreeOfAKind
  | isTwoPair = TwoPair
  | isOnePair = OnePair
  | isHighCard = HighCard
  | otherwise = HighCard
  where
    isFiveOfAKind = a == b && b == c && c == d && d == e
    isFourOfAKind = (a == b && b == c && c == d) || (b == c && c == d && d == e)
    isFullHouse = (a == b && b == c && d == e) || (a == b && c == d && d == e)

-- 100 * combo + card
-- NOTE: card value is not only determined by the first card, but by the whole hand
-- need to do pairwise comparison, then sort the list
calcScore :: Hand -> Int
calcScore hands = 100 * fromEnum (determineCombo hands) + fromEnum (head hands)
