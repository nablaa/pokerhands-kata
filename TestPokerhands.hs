module Main where

import TestCard
import TestHand
import TestQuick

import Test.HUnit
import Data.Maybe
import Text.Printf

test_files = [card_tests, hand_tests]

main = do putStrLn "Running HUnit tests:"
          runTestTT $ TestList $ concat test_files
          putStrLn "\nRunning QuickCheck tests:"
          mapM_ (\(s, a) -> printf "%-25s: " s >> a) quickcheck_tests
          putStrLn "\nAll done."

