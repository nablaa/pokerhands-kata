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
  ("Parse illegal input", "breakingTheTestForFun"),
  ("Parse empty input", "")
  ]

make_parse_test :: String -> String -> Test
make_parse_test name str = TestCase (assertEqual name str (show (fromJust (parseHand str))))

make_parse_test_failing :: String -> String -> Test
make_parse_test_failing name str = TestCase (assertEqual name Nothing (parseHand str))


hand_tests = map (uncurry make_parse_test) show_tests ++ map (uncurry make_parse_test_failing) show_tests_failing

