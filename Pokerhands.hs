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
  show (Card suit value) = undefined