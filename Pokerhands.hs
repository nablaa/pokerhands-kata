module Pokerhands where
-- TODO: exports

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