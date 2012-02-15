module Pokerhands where
-- TODO: exports

import Data.Char

data Card = Card Suit Int
          deriving (Eq)

data Suit = Clubs | Diamonds | Hearts | Spades
          deriving (Eq)
                   
instance Show Suit where
  show Clubs = "C"
  show Diamonds = "D"
  show Hearts = "H"
  show Spades = "S"

instance Show Card where
  show (Card suit value) = valueString value ++ show suit
    where valueString n | n >= 2 && n <= 9 = show n
                        | n == 10 = "T"
                        | n == 11 = "J"
                        | n == 12 = "Q"
                        | n == 13 = "K"
                        | n == 14 = "A"
  
parseCard :: String -> Maybe Card
parseCard [value, suit] | value `elem` ['2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A'] && suit `elem` ['C', 'D', 'H', 'S'] = Just (Card (parseSuit suit) (parseValue value))
                        | otherwise = Nothing
parseCard _ = Nothing

parseValue :: Char -> Int
parseValue 'T' = 10
parseValue 'J' = 11
parseValue 'Q' = 12
parseValue 'K' = 13
parseValue 'A' = 14
parseValue c = digitToInt c

parseSuit :: Char -> Suit
parseSuit 'C' = Clubs
parseSuit 'D' = Diamonds
parseSuit 'H' = Hearts
parseSuit 'S' = Spades