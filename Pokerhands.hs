module Pokerhands where

import Card
import Data.List
import qualified Data.Map as M

data Hand = Hand [Card]
          deriving (Eq)

data HandRank = HighCard | Pair | TwoPairs | ThreeKind | Straight | Flush | FullHouse | FourKind | StraightFlush
              deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance Show Hand where
  show (Hand cards) = intercalate " " (map show cards)

instance Ord Hand where
  compare h1 h2 = undefined

parseHand :: String -> Maybe Hand
parseHand cardStrings = sequence (map parseCard (words cardStrings)) >>= makeHand

makeHand :: [Card] -> Maybe Hand
makeHand cards | length cards /= 5 || length cards /= length (nub cards) = Nothing
makeHand cards = Just $ Hand cards

highestCardValue :: Hand -> Int
highestCardValue (Hand cards) = maximum $ map value cards

getHandRank :: Hand -> HandRank
getHandRank hand = maximum [rank | rank <- [minBound..], contains hand rank]

sameValueCount :: Hand -> Int
sameValueCount (Hand cards) = maximum $ M.elems histogram
  where values = map value cards
        histogram = M.fromListWith (+) $ zip values (repeat 1)

contains :: Hand -> HandRank -> Bool
contains _ HighCard = True

contains hand Pair = sameValueCount hand >= 2
contains hand ThreeKind = sameValueCount hand >= 3
contains hand FourKind = sameValueCount hand >= 4

contains (Hand cards) Straight = values == take 5 (iterate (+1) (head values))
  where values = sort $ map value cards

contains (Hand cards) Flush = length (nub (map suit cards)) == 1
contains hand StraightFlush = contains hand Straight && contains hand Flush

contains (Hand cards) TwoPairs = sort (M.elems histogram) == [1, 2, 2]
  where values = map value cards
        histogram = M.fromListWith (+) $ zip values (repeat 1)

contains (Hand cards) FullHouse = sort (M.elems histogram) == [2, 3]
  where values = map value cards
        histogram = M.fromListWith (+) $ zip values (repeat 1)

