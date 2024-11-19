import Data.List (sort)
import Text.XHtml (h1)

--------- data structures

data Combo = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind deriving (Show, Eq, Ord)

data Card = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Show, Eq, Ord)

newtype Hand = Hand (Card, Card, Card, Card, Card) deriving (Show)

enumDiff :: Enum a -> Enum a -> Int
enumDiff a b = fromEnum a - fromEnum b

instance Ord Hand where
  compare :: Hand -> Hand -> Ordering
  compare (Hand (a1, a2, a3, a4, a5)) (Hand (b1, b2, b3, b4, b5)) = do
    let scoreHand1 = fromEnum a1
    let scoreHand2 = fromEnum b1
    compare hand1 hand2

-- TODO: fixme

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

  print $ h1 == h2
  print $ h1 < h2

--------- solution

determineCombo :: Hand -> Combo

determineCombo hand -- TODO: complete

-- 100 * combo + card
-- NOTE: card value is not only determined by the first card, but by the whole hand
-- need to do pairwise comparison, then sort the list
calcScore :: Hand -> Int
calcScore hands = 100 * fromEnum (determineCombo hands) + fromEnum (head hands)
