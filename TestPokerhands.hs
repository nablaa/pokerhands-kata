module TestPokerhands where

import Pokerhands
import Test.HUnit
import Data.Maybe

show_tests = [
  ("Hearts 4", "4H", Card Hearts 4),
  ("Spades 2", "2S", Card Spades 2),
  ("Spades Ten", "TS", Card Spades 10),
  ("Diamonds Jack", "JD", Card Diamonds 11),
  ("Clubs Queen", "QC", Card Clubs 12),
  ("Hearts King", "KH", Card Hearts 13),
  ("Spades Ace", "AS", Card Spades 14)
  ]

make_test :: (Show a, Eq a) => [Char] -> a -> a -> Test
make_test name expected actual = TestCase (assertEqual ("Testing 'show' (" ++ name ++ ")") expected actual)

make_show_test (name, expected, card) = make_test name expected (show card)
make_read_test (name, input, expected) = make_test name expected (fromJust (parseCard input))

tests = TestList (map make_show_test show_tests ++ map make_read_test show_tests)

main = do runTestTT tests
