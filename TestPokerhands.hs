module TestPokerhands where

import TestCard
import TestHand

import Test.HUnit
import Data.Maybe

test_files = [card_tests, hand_tests]

main = do runTestTT $ TestList $ concat test_files

