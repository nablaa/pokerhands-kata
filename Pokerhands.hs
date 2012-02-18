module Pokerhands (Hand(..), HandRank(..), parseHand, highestCardValue, getHandRank) where

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
  compare hand1 hand2 = case comparison of
                       EQ -> rankCompare rank (handValues hand1) (handValues hand2)
                       _  -> comparison
    where comparison = compare (getHandRank hand1) (getHandRank hand2)
          rank = getHandRank hand1

handValues :: Hand -> [Int]
handValues (Hand cards) = map value cards

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

rankCompare :: HandRank -> [Int] -> [Int] -> Ordering
rankCompare HighCard values1 values2 = compareHighestValues values1 values2
rankCompare Pair values1 values2 = compareKinds 2 values1 values2
rankCompare TwoPairs values1 values2 = compareKinds 2 values1 values2
rankCompare ThreeKind values1 values2 = compareKinds 3 values1 values2
rankCompare Straight values1 values2 = compare (maximum values1) (maximum values2)
rankCompare Flush values1 values2 = compareHighestValues values1 values2
rankCompare FullHouse values1 values2 = compareKinds 3 values1 values2
rankCompare FourKind values1 values2 = compareKinds 4 values1 values2
rankCompare StraightFlush values1 values2 = compareHighestValues values1 values2

compareKinds :: Int -> [Int] -> [Int] -> Ordering
compareKinds kind values1 values2 = case comparison of
                                         EQ -> compareHighestValues (remaining values1) (remaining values2)
                                         _  -> comparison
  where kindValues values = nKindValues kind values
        remaining values = nub values \\ kindValues values
        comparison = compareHighestValues (kindValues values1) (kindValues values2)

compareHighestValues :: [Int] -> [Int] -> Ordering
compareHighestValues l1 l2 = compareHighestValues' (reverse (sort l1)) (reverse (sort l2))

compareHighestValues' :: [Int] -> [Int] -> Ordering
compareHighestValues' [] [] = EQ
compareHighestValues' (x:xs) (y:ys) | x < y = LT
                                    | x > y = GT
                                    | otherwise = compareHighestValues' xs ys

nKindValues :: Int -> [Int] -> [Int]
nKindValues kind list = nub $ fst $ partition (\x -> elemCount x list == kind) list

elemCount :: Eq a => a -> [a] -> Int
elemCount val list = length $ elemIndices val list

