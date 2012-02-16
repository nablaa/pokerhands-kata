module TestHand where

import Pokerhands
import Card
import Test.HUnit
import Data.Maybe

show_tests = [
  ("Parse normal 1", "4H JC AS KD 2S"),
  ("Parse normal 2", "2C 3D 4H 5S 6C")
  ]

show_tests_failing = [
  ("Parse too few cards", "4H JC AS KD"),
  ("Parse too many cards", "4H JC AS KD 2S 5H"),
  ("Parse illegal card", "4H JC 1S KD 2S"),
  ("Parse illegal card", "4H JC 1S KD 1S"),
  ("Parse illegal input", "breakingTheTestForFun"),
  ("Parse empty input", "")
  ]

make_parse_test :: String -> String -> Test
make_parse_test name str = TestCase (assertEqual name str (show (fromJust (parseHand str))))

make_parse_test_failing :: String -> String -> Test
make_parse_test_failing name str = TestCase (assertEqual name Nothing (parseHand str))


highest_card_tests = [
    ("Highest card", 2, "2H 2C 2S 2D 2H")
  , ("Highest card", 8, "2H 5C 3D 7C 8H")
  , ("Highest card", 10, "4H 5C TD 7C 8H")
  , ("Highest card", 11, "4H JC TD 7C 8H")
  , ("Highest card", 12, "4H JC TD QC 8H")
  , ("Highest card", 13, "4H JC TD 7C KH")
  , ("Highest card", 14, "AH JC TD 7C KH")
  ]

make_highest_card_test :: (String, Int, String) -> Test
make_highest_card_test (name, expected, input) = TestCase (assertEqual name expected (highestCardValue (fromJust (parseHand input))))

hand_tests = map (uncurry make_parse_test) show_tests
          ++ map (uncurry make_parse_test_failing) show_tests_failing
          ++ map make_highest_card_test highest_card_tests
