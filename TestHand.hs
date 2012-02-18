module TestHand where

import Pokerhands
import Card
import Test.HUnit
import Data.Maybe

show_tests = [
    ("Parse normal 1", "4H JC AS KD 2S")
  , ("Parse normal 2", "2C 3D 4H 5S 6C")
  ]

show_tests_failing = [
    ("Parse too few cards", "4H JC AS KD")
  , ("Parse too many cards", "4H JC AS KD 2S 5H")
  , ("Parse illegal card", "4H JC 1S KD 2S")
  , ("Parse illegal card", "4H JC 1S KD 1S")
  , ("Parse illegal input", "breakingTheTestForFun")
  , ("Parse empty input", "")
  , ("Parse hand with two identical cards", "6S AS JC 6S 5H")
  ]

make_parse_test :: String -> String -> Test
make_parse_test name str = TestCase (assertEqual name str (show (fromJust (parseHand str))))

make_parse_test_failing :: String -> String -> Test
make_parse_test_failing name str = TestCase (assertEqual name Nothing (parseHand str))


highest_card_tests = [
    ("Highest card", 3, "2H 2C 3S 2D 2S")
  , ("Highest card", 8, "2H 5C 3D 7C 8H")
  , ("Highest card", 10, "4H 5C TD 7C 8H")
  , ("Highest card", 11, "4H JC TD 7C 8H")
  , ("Highest card", 12, "4H JC TD QC 8H")
  , ("Highest card", 13, "4H JC TD 7C KH")
  , ("Highest card", 14, "AH JC TD 7C KH")
  ]

make_highest_card_test :: (String, Int, String) -> Test
make_highest_card_test (name, expected, input) = TestCase (assertEqual name expected (highestCardValue (fromJust (parseHand input))))


hand_rank_tests = [
    ("Highest card", HighCard, "4H 9C KS JC 8S")
  , ("Pair", Pair, "4H 9C KS 4S 8S")
  , ("Two pairs", TwoPairs, "4H 9C KS 4S 9S")
  , ("Three of a kind", ThreeKind, "4H 9C 4S 4D JC")
  , ("Straight", Straight, "4H 5S 6D 7H 8D")
  , ("Flush", Flush, "4S 8S 2S 9S AS")
  , ("Full house", FullHouse, "6H 6S 2H 2S 2C")
  , ("Four of a kind", FourKind, "6H 2D 2H 2S 2C")
  , ("Straight flush", StraightFlush, "8H 9H TH JH QH")
  ]

make_rank_test :: (String, HandRank, String) -> Test
make_rank_test (name, expected, input) = TestCase (assertEqual name expected (getHandRank (fromJust (parseHand input))))

hand_comparison_tests = [
  -- Simple cases
    ("Loses: High card", LT, "2H 3D 5S 9C KD", "2C 3H 4S 8C AH")
  , ("Loses: Flush", LT, "2H 4S 4C 2D 5H", "2S 8S AS QS 3S")
  , ("Wins: High card", GT, "2H 3D 5S 9C KD", "2C 3H 4S 8C KH")
  , ("Tie", EQ, "2H 3D 5S 9C KD", "2D 3H 5C 9S KH")

  -- High card
  , ("High card", GT, "2H 5D 7C 9S JC", "2H 5D 7C 9S TC")
  , ("High card, second card", GT, "2H 5D 7C 9S JC", "2H 5D 7C 8S JC")
  , ("High card, third card", GT, "2H 5D 8C 9S JC", "2H 5D 7C 9S JC")
  , ("High card, fourth card", GT, "2H 6D 7C 9S JC", "2H 5D 7C 9S JC")
  , ("High card, fifth card", GT, "3H 5D 7C 9S JC", "2H 5D 7C 9S JC")
  , ("High card tie", EQ, "2H 5D 7C 9S JC", "2H 5C 7C 9H JS")

  -- Pair
  , ("Pair, against high card", GT, "9H 9D 6C 5D 3S", "TH 9D 6C 5D 3S")
  , ("Pair, highest pair", GT, "9H 9D 6C 5D 3S", "8H 8D 6C 5D 3S")
  , ("Pair, same, first highest card", GT, "9H 9D 7C 5D 3S", "9H 9D 6C 5D 3S")
  , ("Pair, same, second highest card", GT, "9H 9D 6C 5D 3S", "9H 9D 6C 4D 3S")
  , ("Pair, same, third highest card", GT, "9H 9D 6C 5D 3S", "9H 9D 6C 5D 2S")
  , ("Pair, tie", EQ, "9H 9D 6C 5D 3S", "9H 9D 6H 5S 3C")

  -- Two Pairs
  , ("Two pairs, against pair", GT, "8H 8C 5C 5D QC", "AS AD QH JS TD")
  , ("Two pairs, highest pair", GT, "8H 8C 5C 5D QC", "7S 7D 6H 6S AD")
  , ("Two pairs, second highest pair", GT, "8H 8C 5C 5D QC", "8S 8D 4H 4S AD")
  , ("Two pairs, remaining card", GT, "8H 8C 5C 5D QC", "8S 8D 5H 5S JD")
  , ("Two pairs, tie", EQ, "8H 8C 5C 5D QC", "8S 8D 5H 5S QD")

  -- Three of a kind
  , ("Three of a kind, against two pairs", GT, "8C 8H 8D 4C 3S", "AD AH QH QD KC")
  , ("Three of a kind, highest value", GT, "8C 8H 8D 4C 3S", "7H 7C 7D KC AS")
  , ("Three of a kind, remaining first highest card", GT, "8C 8H 8D TC 3S", "8H 8C 8D 9C 7S")
  , ("Three of a kind, remaining second highest card", GT, "8C 8H 8D TC 7S", "8H 8C 8D TC 6S")
  , ("Three of a kind, tie", EQ, "8C 8H 8D TC 6S", "8H 8C 8D TC 6S")

  -- Straight
  , ("Straight, against three of a kind", GT, "5H 6D 7C 8S 9S", "AC AD AS KS QD")
  , ("Straight, highest card", GT, "5H 6D 7C 8S 9S", "4D 5H 6D 7C 8C")
  , ("Straight, tie", EQ, "5H 6D 7C 8S 9S", "5C 6H 7C 8C 9S")

  -- Flush
  , ("Flush, against straight", GT, "5S 9S KS 2S 4S", "AC KS QH JC TH")
  , ("Flush, first highest card", GT, "5S 9S KS 2S 4S", "5S 9S QS 2S 4S")
  , ("Flush, second highest card", GT, "5S 9S KS 2S 4S", "5S 8S KS 2S 4S")
  , ("Flush, third highest card", GT, "6S 9S KS 2S 4S", "5S 9S KS 2S 4S")
  , ("Flush, fourth highest card", GT, "5S 9S KS 2S 4S", "5S 9S KS 2S 3S")
  , ("Flush, fifth highest card", GT, "5S 9S KS 3S 4S", "5S 9S KS 2S 4S")
  , ("Flush, tie", EQ, "5S 9S KS 2S 4S", "5S 9S KS 2S 4S")

  -- Full house
  , ("Full house, against flush", GT, "8C 8S 8H 4C 4S", "AC KS QH JC TH")
  , ("Full house, higher three cards", GT, "8C 8S 8H 4C 4S", "7D 7S 7H KC KD")
  , ("Full house, same three cards, higher two cards", GT, "8C 8S 8H 5C 5S", "8D 8S 8H 4D 4S")
  , ("Full house, tie", EQ, "8C 8S 8H 5C 5S", "8D 8S 8H 5D 5S")

  -- Four of a kind
  , ("Four of a kind, against full house", GT, "2H 2C 2D 2S 3C", "AC AS AH KC KS")
  , ("Four of a kind, highest value", GT, "5H 5C 5D 5S 2C", "4H 4C 4D 4S AS")
  , ("Four of a kind, higher remaining card", GT, "5H 5C 5D 5S 8C", "5S 5H 5D 5C 7S")
  , ("Four of a kind, tie", EQ, "5H 5C 5D 5S 8C", "5S 5H 5D 5C 8S")

  -- Straight flush
  , ("Straight flush, against four of a kind", GT, "4S 5S 6S 7S 8S", "AC AD AH AS KC")
  , ("Straight flush, highest card", GT, "4S 5S 6S 7S 8S", "3S 4S 5S 6S 7S")
  , ("Straight flush, tie", EQ, "4S 5S 6S 7S 8S", "4S 5S 6S 7S 8S")
  ]


make_comparison_test :: (String, Ordering, String, String) -> Test
make_comparison_test (name, expected, input1, input2) = TestCase (assertEqual name expected (compare hand1 hand2))
  where hand1 = fromJust $ parseHand input1
        hand2 = fromJust $ parseHand input2

hand_tests = map (uncurry make_parse_test) show_tests
          ++ map (uncurry make_parse_test_failing) show_tests_failing
          ++ map make_highest_card_test highest_card_tests
          ++ map make_rank_test hand_rank_tests
          ++ map make_comparison_test hand_comparison_tests

