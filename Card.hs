module Card (Card(..), Suit(..), parseCard) where

import Data.Char
import Data.Maybe

data Card = Card Suit Int
          deriving (Eq)

data Suit = Clubs | Diamonds | Hearts | Spades
          deriving (Eq)

suitChars = [(Clubs, 'C'), (Diamonds, 'D'), (Hearts, 'H'), (Spades, 'S')]
valueChars = [(n, intToDigit n) | n <- [2..9]] ++ [(10, 'T'), (11, 'J'), (12, 'Q'), (13, 'K'), (14, 'A')]

instance Show Suit where
  show suit = [fromJust $ lookup suit suitChars]

instance Show Card where
  show (Card suit value) = fromJust (lookup value valueChars) : show suit


parseCard :: String -> Maybe Card
parseCard [v, s] | v `elem` legalValues && s `elem` legalSuits = Just (Card suit value)
  where rlookup x = lookup x . map swap
        swap (x, y) = (y, x)
        (_, legalSuits) = unzip suitChars
        (_, legalValues) = unzip valueChars
        suit = fromJust $ rlookup s suitChars
        value = fromJust $ rlookup v valueChars
parseCard _ = Nothing


