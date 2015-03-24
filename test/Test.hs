module Main where

import Test.Tasty
import Test.Tasty.HUnit

import PixeloSolver.Game.Nonogram.Tests

hunitTests = allTests

tests = testGroup "Unit tests" [allTests]

main = defaultMain tests
