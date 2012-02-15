module TestPokerhands where

import Pokerhands
import Test.HUnit

test_show = TestCase (assertEqual "Testing show (Hearts 4)" "4H" (show (Card Hearts 4)))

tests = TestList [test_show]

main = do runTestTT tests