module Pokerhands where

import Card
import Data.List

data Hand = Hand [Card]
          deriving (Eq)

instance Show Hand where
  show (Hand cards) = intercalate " " (map show cards)

parseHand :: String -> Maybe Hand
parseHand cardStrings = sequence (map parseCard (words cardStrings)) >>= makeHand

makeHand :: [Card] -> Maybe Hand
makeHand cards | length cards /= 5 || length cards /= length (nub cards) = Nothing
makeHand cards = Just $ Hand cards

highestCardValue :: Hand -> Int
highestCardValue (Hand cards) = maximum $ map value cards


