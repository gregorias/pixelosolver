{-# LANGUAGE FlexibleContexts #-}
-- Make definition of constants explicit, especially the interDigitSpace.
-- Stripe length should be calculated more smartly, or by cutting it.
--Fix crashses when taking screenshot without board on it.
-- Decrease memory footprint
-- Optimize, especially board finding
-- Finish recognition algorithm for 6 and 9
-- Fix recognition of double digits like 20 which do not have space in between.
--
--ghc --make Main -package-db=.cabal-sandbox/x86_64-linux-ghc-7.6.3-packages.conf.d/ -rtsopts
module Main(
  module Main
  , module PixeloSolver.Data.Graphics
  , module PixeloSolver.Graphics.Utils
  , initGUI
  ) where

import Control.Arrow
import Control.Concurrent
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Array
import Graphics.UI.Gtk

import PixeloSolver.AI.OCR
import PixeloSolver.Data.Array.Extra
import PixeloSolver.Data.Graphics
import PixeloSolver.Game.Nonogram
import PixeloSolver.Graphics.Utils

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
  let maybePixeloBoard = findPixeloBoard colorMap
  case maybePixeloBoard of 
    Nothing -> throwE "Couldn't find Pixelo's board."
    Just pixeloBoard -> return pixeloBoard

screenshotToBoardAndGame :: Map m r RGB => m RGB 
  -> ExceptT String IO (PixeloBoard, PixeloGame)
screenshotToBoardAndGame colorMap = do
  pixeloBoard <- ExceptT . return . runExcept . screenshotToBoard $ colorMap
  pixeloGame <- lift $ colorMapToPixeloGame colorMap pixeloBoard
  return (pixeloBoard, pixeloGame)

solveGame :: (PixeloBoard, PixeloGame) -> Except String PixeloGame
solveGame (pixeloBoard, unsolvedGame) = do
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
pipeline :: ExceptT String IO ()
pipeline = lift getScreenshot
  >>= screenshotToBoardAndGame 
  >>= (runKleisli $ kleisliFst &&& kleisliSolveGame) 
  >>= (lift . uncurry playGame)
  where
    kleisliSolveGame = Kleisli $ ExceptT . return . runExcept . solveGame
      :: Kleisli (ExceptT String IO) (PixeloBoard, PixeloGame) PixeloGame
    kleisliFst = Kleisli $ return . fst

-- | Runs the pipeline and prints errors if any.
run :: IO ()
run = do
  result <- runExceptT pipeline
  case result of
    Left errorMsg -> putStrLn errorMsg
    _ -> return ()

main :: IO ()
main = showButtonDialog run
