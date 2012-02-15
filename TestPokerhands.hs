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

make_show_test :: (String, String, Card) -> Test
make_show_test (name, expected, card) = TestCase (assertEqual ("Testing 'show' (" ++ name ++ ")") expected (show card))

make_read_test :: (String, String, Card) -> Test
make_read_test (name, input, expected) = TestCase (assertEqual ("Testing 'read' (" ++ name ++ ")") expected (fromJust (parseCard input)))

tests = TestList (map make_show_test show_tests ++ map make_read_test show_tests)


main = do runTestTT tests
