module Main where

import Game.Nonogram.Tests
import Test.Tasty
import Test.Tasty.HUnit

hunitTests = allTests

tests = testGroup "Unit tests" [allTests]

main = defaultMain tests
