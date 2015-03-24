module PixeloSolver.Game.Nonogram.Tests(
  allTests
  ) where

import Data.List
import Test.Tasty
import Test.Tasty.HUnit

import PixeloSolver.Game.Nonogram

allTests = testGroup "Game.Nonogram tests" 
  [shouldShowGameCorrectly
    , shouldGenerateAllSolutions
    , shouldGenerateAllSolutions2
    , shouldGenerateAllSolutions3
    , shouldGenerateOnlyOneEmptySolution
    , shouldGenerateEmptySolution
    ]

shouldShowGameCorrectly = testCase "shouldShowGameCorrectly" 
  $ expectedString @=? show game
  where
    expectedString = 
      "      1 \n"
      ++ "      0 \n"
      ++ "        \n"
      ++ "        \n"
      ++ "      11\n"
      ++ "        \n"
      ++ "10  1 ??\n"
      ++ " 1  1 ??\n"
    rowHints = [[10, 1], [1, 1]]
    colHints = [[10, 1], [1]]
    game = PixeloGame (emptyGameBoard 2 2) rowHints colHints

shouldGenerateAllSolutions = testCase "shouldGenerateAllSolutions"
  $ expectedSolutions @=? generateSolutions hints tiles
  where 
    expectedSolutions = [[Full, Full, Empty, Full, Full]]
    hints = [2, 2]
    tiles = [Done Full, Unknown, Unknown, Unknown, Unknown]

shouldGenerateAllSolutions2 = testCase "shouldGenerateAllSolutions2"
  $ assertBool "shouldGenerateAllSolutions" 
    (length expectedSolutions == length generatedSolutions 
      && null (expectedSolutions \\ generatedSolutions))
  where 
    expectedSolutions = [
      [Full, Full, Empty, Full, Full, Empty]
      , [Full, Full, Empty, Empty, Full, Full]
      , [Empty, Full, Full, Empty, Full, Full]]
    hints = [2, 2]
    tiles = [Unknown, Unknown, Unknown, Unknown, Unknown, Unknown]
    generatedSolutions = generateSolutions hints tiles

shouldGenerateAllSolutions3 = testCase "shouldGenerateAllSolutions3"
  $ assertBool "shouldGenerateAllSolutions" (null generatedSolutions)
  where 
    hints = [2, 2]
    tiles = [Done Empty, Done Full, Unknown, Unknown, Done Full, Done Empty]
    generatedSolutions = generateSolutions hints tiles

shouldGenerateOnlyOneEmptySolution = testCase
  "shouldGenerateOnlyOneEmptySolution"
  $ assertBool "shouldGenerateOnlyOneEmptySolution"
    (expectedSolutions == generatedSolutions)
  where 
    expectedSolutions = [[Empty, Empty]]
    hints = [0]
    tiles = [Unknown, Unknown]
    generatedSolutions = generateSolutions hints tiles

shouldGenerateEmptySolution = testCase
  "shouldGenerateEmptySolution"
  $ assertBool "shouldGenerateEmptySolution"
    (expectedSolutions == generatedSolutions)
  where 
    expectedSolutions = [[Empty, Empty]]
    hints = [0]
    tiles = [Done Empty, Done Empty]
    generatedSolutions = generateSolutions hints tiles
