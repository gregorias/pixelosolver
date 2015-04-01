{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Main(
  main
  , runStatic
  , runSet
  , runSets
  ) where

import Control.Arrow
import Control.Concurrent
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Array
import Data.Maybe

import PixeloSolver.AI.OCR
import PixeloSolver.Data.Graphics
import PixeloSolver.Game.Nonogram
import PixeloSolver.Graphics.Utils

-- Constants used by OCR

distanceTolerance :: TileDistanceTolerance
distanceTolerance = 6

minimalTileLength :: MinimalTileLength
minimalTileLength = 15

findBoardConstants :: FindBoardConstants
findBoardConstants = (distanceTolerance, minimalTileLength)

numberTolerances :: NumberTolerances
numberTolerances = NT 26 13

emptyStripeSpaceTolerance :: EmptyStripeSpaceTolerance
emptyStripeSpaceTolerance = 35

ocrConstants :: OCRConstants
ocrConstants = OCRConstants emptyStripeSpaceTolerance numberTolerances

-- | Clicks fills on the screen using pixelo's board coordinates and
-- solved game.
playGame :: PixeloBoard -> PixeloGame -> IO ()
playGame pxBoard solvedGame =
  sequence_ $ map processGameTile (assocs gameBoard)
  where
    gameBoard = pixeloGameGetBoard solvedGame
    rowPositions = pixeloBoardGetRows pxBoard
    colPositions = pixeloBoardGetColumns pxBoard
    tileWidth = pixeloBoardGetTileWidth pxBoard

    processGameTile :: ((Int, Int), PixeloTile) -> IO ()
    processGameTile ((y, x), Done Full) = do
      let (mouseX, mouseY) = (fst (colPositions !! x) + (tileWidth `div` 2),
                              fst (rowPositions !! y) + (tileWidth `div` 2))
      clickMouseButtonAt mouseX mouseY
      threadDelay 10000

    processGameTile _ = return ()

-- Main pipeline functions written as Kleisli functions

screenshotToBoard :: Map m r RGB => m RGB -> Except String PixeloBoard
screenshotToBoard colorMap = do
  let maybePixeloBoard = findPixeloBoard findBoardConstants colorMap
  case maybePixeloBoard of
    Nothing -> throwE "Couldn't find Pixelo's board."
    Just pixeloBoard -> return pixeloBoard

-- | Perform OCR to find the Nonogram game. Uses human input if unsure.
screenshotToBoardAndGame :: Map m r RGB => m RGB
  -> ExceptT String IO (PixeloBoard, PixeloGame)
screenshotToBoardAndGame colorMap = do
  pixeloBoard <- ExceptT . return . runExcept . screenshotToBoard $ colorMap
  pixeloGame <- lift $ screenshotToPixeloGame ocrConstants colorMap pixeloBoard
  return (pixeloBoard, pixeloGame)

-- | Perform OCR to find the Nonogram game. Returns exception if couldn't
-- recognize the game.
screenshotToBoardAndGameAuto :: Map m r RGB => m RGB
  -> ExceptT String IO (PixeloBoard, PixeloGame)
screenshotToBoardAndGameAuto colorMap = do
  pixeloBoard <- ExceptT . return . runExcept . screenshotToBoard $ colorMap
  pixeloGame <- fromMaybe (throwE "Couldn't recognize all hints.")
    (fmap return $ screenshotToPixeloGameAuto ocrConstants colorMap pixeloBoard)
  return (pixeloBoard, pixeloGame)

solveGame :: PixeloGame -> Except String PixeloGame
solveGame unsolvedGame = do
  let maybeSolvedGame = solvePixeloGame unsolvedGame
  case maybeSolvedGame of
    Nothing -> throwE $ "Game is unsolvable. The board is: \n"
      ++ show unsolvedGame
    Just solvedGame -> return solvedGame

-- | The main action pipeline with possible error output which combines:
--
--   1. Taking a screenshot
--
--   2. Finding the board on the screenshot and performing OCR to get PixeloGame
--
--   3. Solving the game
--
--   4. Playing the solved game
--
-- This function asks user for input in case it can't recognize a hint
pipeline :: ExceptT String IO ()
pipeline = pipeline' screenshotToBoardAndGame

-- | Same as pipeline, but simply outputs an error message OCR failure.
pipelineAuto :: ExceptT String IO ()
pipelineAuto = pipeline' screenshotToBoardAndGameAuto

pipeline' ::
  (forall m r . (Map m r RGB) => m RGB -> ExceptT String IO (PixeloBoard, PixeloGame))
  -> ExceptT String IO ()
pipeline' screenshotToBoardAndGameFunction = lift getScreenshot
  >>= screenshotToBoardAndGameFunction
  >>= (runKleisli $ kleisliFst &&& kleisliSolveGame)
  >>= (lift . uncurry playGame)
  where
    kleisliSolveGame = Kleisli $ ExceptT . return . runExcept . solveGame . snd
      :: Kleisli (ExceptT String IO) (PixeloBoard, PixeloGame) PixeloGame
    kleisliFst = Kleisli $ return . fst

-- | Runs the pipeline and prints errors if any.
run :: IO ()
run = do
  result <- runExceptT pipeline
  case result of
    Left errorMsg -> putStrLn errorMsg
    _ -> return ()

-- | Given coordinates of major buttons this function solves a set of puzzles
runSet :: [(Int, Int)] -- ^ (y,x) coordinates of puzzle buttons in the set menu.
  -> (Int, Int) -- ^ Position of the next button on the summary page.
  -> (Int, Int) -- ^ Position of the menu button on the puzzle page.
  -> (Int, Int) -- ^ Position of the give up button in the menu
  -> Int -- ^ Delay betwen clicks
  -> Int -- ^ Delay for the clear screen to load up
  -> Int -- ^ Longer delay for waiting for the summary page to load up
         -- completely
  -> IO ()
runSet puzzlePositions
  nextPosition
  menuPosition
  backPosition
  clickDelay
  clearDelay
  nextScrenDelay =
  sequence_ $ map singleRun puzzlePositions
  where
    singleRun (yPuzzle, xPuzzle) = do
      clickMouseButtonAt xPuzzle yPuzzle
      threadDelay clickDelay
      clickMouseButtonAt xPuzzle yPuzzle
      threadDelay clickDelay
      result <- runExceptT pipelineAuto
      threadDelay clearDelay
      case result of
        Left errorMsg -> do
          putStrLn errorMsg
          clickMouseButtonAt (snd menuPosition) (fst menuPosition)
          threadDelay clickDelay
          clickMouseButtonAt (snd backPosition) (fst backPosition)
        _ -> do
          clickMouseButtonAt (snd nextPosition) (fst nextPosition)
          threadDelay nextScrenDelay
          clickMouseButtonAt (snd menuPosition) (fst menuPosition)
          threadDelay clickDelay
          clickMouseButtonAt (snd nextPosition) (fst nextPosition)
      threadDelay clickDelay

-- \ Runs runSet function many times. Takes the same arguments except the first
-- two.
runSets :: Int -- ^ How many times should the runSet function be called
  -> (Int, Int) -- ^ Position of the next set arrow
  -> [(Int, Int)]
  -> (Int, Int)
  -> (Int, Int)
  -> (Int, Int)
  -> Int
  -> Int
  -> Int
  -> IO ()
runSets setCount
  nextSetPosition
  puzzlePositions
  nextPosition
  menuPosition
  backPosition
  clickDelay
  clearDelay
  nextScrenDelay =
  sequence_
  $ replicate setCount (do
      (runSet
        puzzlePositions
        nextPosition
        menuPosition
        backPosition
        clickDelay
        clearDelay
        nextScrenDelay)
      threadDelay clickDelay
      clickMouseButtonAt (snd nextSetPosition) (fst nextSetPosition)
      threadDelay clickDelay)



main :: IO ()
main = showButtonDialog run

-- | Pipeline which solves a game stored in an image and prints the solution to
-- the console.
pipelineStatic :: FilePath -> ExceptT String IO ()
pipelineStatic filePath =
  lift (getImageFromFile filePath)
  >>= screenshotToBoardAndGame
  >>= ExceptT . return . runExcept . solveGame . snd
  >>= lift . putStrLn . show

runStatic :: FilePath -> IO ()
runStatic filePath = do
  eitherResult <- runExceptT $ pipelineStatic filePath
  case eitherResult of
    Left err -> putStrLn $ "Error: " ++ err
    _ -> return ()
