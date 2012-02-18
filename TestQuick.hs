module TestQuick where

import Card
import Pokerhands
import Data.List
import Test.QuickCheck

deck :: [Card]
deck = [Card s v | s <- [Clubs, Diamonds, Hearts, Spades], v <- [2..14]]

instance Arbitrary Hand where
  arbitrary = do c1 <- elements deck
                 c2 <- elements $ deck \\ [c1]
                 c3 <- elements $ deck \\ [c1, c2]
                 c4 <- elements $ deck \\ [c1, c2, c3]
                 c5 <- elements $ deck \\ [c1, c2, c3, c4]
                 return $ Hand [c1, c2, c3, c4, c5]

prop_comparison_symmetry :: Hand -> Hand -> Bool
prop_comparison_symmetry hand1 hand2 = case compare hand1 hand2 of
                                            GT -> compare hand2 hand1 == LT
                                            LT -> compare hand2 hand1 == GT
                                            EQ -> compare hand2 hand1 == EQ

quickcheck_tests = [
    ("comparison.symmetry", quickCheck prop_comparison_symmetry)
  ]

