module PixeloSolver.Game.Nonogram where

import Control.Arrow
import Control.Monad
import Data.Array
import Text.Printf

import PixeloSolver.Data.Array.Extra

-- Data and Show instance definitions

-- | Single tile of Nonogram board
data PixeloTile = Done PixeloTileFill | Unknown deriving Eq

instance Show PixeloTile where
  show (Done Empty) = "_"
  show (Done Full) = "#"
  show Unknown = "?"

data PixeloTileFill = Empty | Full deriving (Eq, Show)

data PixeloGame = PixeloGame { 
  pixeloGameGetBoard :: (Array (Int, Int) PixeloTile),
  pixeloGameGetRowHints :: [[Int]] ,
  pixeloGameGetColHints :: [[Int]]
} deriving (Eq)

instance Show PixeloGame where
  show (PixeloGame board rowHints columnHints) =
    (charArrayToColumnHintsString rowHintsStringLength . columnHintsToCharArray 
      $ columnHints) 
    ++ (concat . map (showRow rowHintsStringLength) 
      $ (map (\i -> (rowHints !! i, getRow i board)) [0..height]))
    where
      height = fst . snd . bounds $ board
      rowHintsFieldLength = getMaxHintFieldLength rowHints

      rowHintsStringLength :: Int
      rowHintsStringLength = maximum 
        . map (hintsStringLength rowHintsFieldLength) 
        $ rowHints

      showRow :: Int -> ([Int], Array Int PixeloTile) -> String
      showRow rowHintsStringLength = 
        (++ "\n")
        . (uncurry (++))
        . (showRowHints rowHintsStringLength *** showBoardRow)
      showRowHints :: Int -> [Int] -> String
      showRowHints rowHintsStringLength row = 
        replicate prefixLength ' ' ++ hintsToString rowHintsFieldLength row
        where 
          prefixLength = rowHintsStringLength 
            - hintsStringLength rowHintsFieldLength row

      showBoardRow :: Array Int PixeloTile -> String
      showBoardRow = concat . map show . elems

intLength :: Int -> Int
intLength = ceiling . (logBase 10) . fromIntegral . (+ 1)

hintFieldLength :: Int -> Int
hintFieldLength = (+ 1) . intLength

getMaxHintFieldLength :: [[Int]] -> Int
getMaxHintFieldLength = maximum . map (maximum . map hintFieldLength)

hintsStringLength :: Int -> [Int] -> Int
hintsStringLength fieldLength = (* fieldLength) . length

columnHintsToCharArray :: [[Int]] -> Array (Int, Int) Char
columnHintsToCharArray hss = 
  initialArray // assocs
  where
    maxHintFieldLength = getMaxHintFieldLength hss
    height = maximum 
      . map (hintsStringLength maxHintFieldLength) 
      $ hss
    width = length hss
    initialArray = listArray
      ((0, 0), (height - 1, width - 1))
      (replicate (width * height) ' ')
    columnStrings = map (hintsToString maxHintFieldLength) hss
    assocs = concat 
      . map 
        (\(x, columnString) -> 
          let
            prefixLength = height - length columnString
            enumString = zip [0..] columnString
          in
            map (\(i, c) -> ((prefixLength + i, x), c)) enumString)
      $ zip [0..] columnStrings

charArrayToColumnHintsString :: Int -> Array (Int, Int) Char -> String
charArrayToColumnHintsString = charArrayToColumnHintsString' (0, 0)

charArrayToColumnHintsString' :: (Int, Int) -> Int -> Array (Int, Int) Char -> String
charArrayToColumnHintsString' (y, x) prefixLength charArray =
  if y > height 
  then ""
  else
    if x == 0
    then replicate prefixLength ' '
      ++ charArrayToColumnHintsString' (y, 1) prefixLength charArray
    else
      if x > width + 1
      then '\n' : charArrayToColumnHintsString' (y + 1, 0) prefixLength
        charArray
      else (charArray ! (y, x - 1)) 
        : charArrayToColumnHintsString' (y, x + 1) prefixLength charArray
  where
    (height, width) = snd . bounds $ charArray

hintsToString :: Int -> [Int] -> String
hintsToString fieldLength [] = ""
hintsToString fieldLength (h : hs) = 
  (prefix ++ hint ++ " ") ++ hintsToString fieldLength hs
  where 
    hint = show h
    prefixLength = fieldLength - 1 - length hint
    prefix = replicate prefixLength ' '
      
prettyPrintBoard :: Array (Int, Int) PixeloTile -> String
prettyPrintBoard board = concat 
  $ map (\i -> concat (map show $ elems (getRow i board)) ++ ['\n']) [0..height]
  where
    height = snd . snd . bounds $ board

-- Solver functions

-- | generate possible solutions for a row/columns
generateSolutions :: [Int] -- ^ hints for this row/column
  -> [PixeloTile] -- ^ partially filled row/column
  -> [[PixeloTileFill]] -- ^ possible solutions
generateSolutions hs cs = generateSolutions' False 0 hs cs

--generateSolutions' isStreak curStreak restOfSpec constraints
generateSolutions' :: Bool -> Int -> [Int] -> [PixeloTile] -> [[PixeloTileFill]]
generateSolutions' False _ [] [] = [[]]
generateSolutions' False _ [] (c : cs) =
  case c of
    Done Full -> []
    _ -> do 
      sol <- generateSolutions' False 0 [] cs
      return $ Empty : sol
generateSolutions' False _ [0] [] = [[]]
generateSolutions' False _ [0] (c : cs) = 
  case c of
    Done Full -> []
    _ -> do
      sol <- generateSolutions' False 0 [0] (cs)
      return $ Empty : sol
generateSolutions' False _ (s : ss) [] = []
generateSolutions' False _ (s : ss) (c : cs) = 
  case c of
    Done Full -> generateSolutions' True s ss (c : cs)
    Done Empty -> do
      sol <- generateSolutions' False 0 (s : ss) (cs)
      return $ Empty : sol
    Unknown -> 
      (do
        restOfSol <- generateSolutions' False 0 (s : ss) (cs)
        return $ Empty : restOfSol)
      ++ (generateSolutions' True s ss (c : cs))
generateSolutions' True 0 [] [] = [[]]
generateSolutions' True _ _ [] = []
generateSolutions' True 0 ss (c : cs) = 
  case c of
    Done Full -> []
    _ -> do
      sol <- generateSolutions' False 0 ss cs
      return $ Empty : sol
generateSolutions' True s ss (c : cs) =
  case c of
    Done Empty -> []
    _ -> do
      sol <- generateSolutions' True (s - 1) ss cs
      return $ Full : sol

-- | Given solutions return their intersection
mergeSolutions :: [[PixeloTileFill]] -> [PixeloTile]
mergeSolutions [] = undefined
mergeSolutions (s : ss) = mergeSolutions' (map Done s) ss

mergeSolutions' :: [PixeloTile] -> [[PixeloTileFill]] -> [PixeloTile]
mergeSolutions' constraints [] = constraints
mergeSolutions' constraints (s : ss) = 
  if all (== Unknown) constraints
  then constraints
  else mergeSolutions' (mergeSolution constraints s) ss

mergeSolution :: [PixeloTile] -> [PixeloTileFill] -> [PixeloTile]
mergeSolution [] [] = []
mergeSolution (Unknown : cs) (s : ss) = Unknown : (mergeSolution cs ss)
mergeSolution (Done a : cs) (s : ss) =
  if a == s
  then Done a : (mergeSolution cs ss)
  else Unknown : (mergeSolution cs ss)
mergeSolution _ _ = undefined

-- | Solve given row/column as much as possible. Return Nothing if solution is
-- impossible
stepSolvePixeloGameDim ::
  (Int -> Array (Int, Int) PixeloTile -> Array Int PixeloTile)
  -> (Int 
    -> Array Int PixeloTile 
    -> Array (Int, Int) PixeloTile 
    -> Array (Int, Int) PixeloTile)
  -> (PixeloGame -> [[Int]])
  -> Int
  -> PixeloGame
  -> Maybe PixeloGame
stepSolvePixeloGameDim getDim setDim getHints dimId game =   
  case generatedSolutions of
    [] -> Nothing
    _ -> Just 
      $ PixeloGame
        newBoard 
        (pixeloGameGetRowHints game)
        (pixeloGameGetColHints game)
  where
    dimHints = getHints game !! dimId
    board = pixeloGameGetBoard game
    dimList = elems $ getDim dimId board
    generatedSolutions = generateSolutions dimHints dimList
    newDimList = mergeSolutions generatedSolutions
    newDim = listArray (0, length dimList - 1)  newDimList
    newBoard = setDim dimId newDim board

stepSolvePixeloGameRow :: Int -> PixeloGame -> Maybe PixeloGame
stepSolvePixeloGameRow =
  stepSolvePixeloGameDim getRow setRow pixeloGameGetRowHints

stepSolvePixeloGameCol :: Int -> PixeloGame -> Maybe PixeloGame
stepSolvePixeloGameCol =   
  stepSolvePixeloGameDim getColumn setColumn pixeloGameGetColHints

stepSolvePixeloGameDims ::
  (Int -> PixeloGame -> Maybe PixeloGame)
  -> Int 
  -> PixeloGame 
  -> Maybe PixeloGame
stepSolvePixeloGameDims stepDimSolve dimSize = 
  foldr1 (>=>) funs
  where
    funs = map stepDimSolve [0..dimSize]

stepSolvePixeloGameRows :: PixeloGame -> Maybe PixeloGame
stepSolvePixeloGameRows game =
  stepSolvePixeloGameDims stepSolvePixeloGameRow height game
  where
    height = fst . snd . bounds $ pixeloGameGetBoard game

stepSolvePixeloGameCols :: PixeloGame -> Maybe PixeloGame
stepSolvePixeloGameCols game =
  stepSolvePixeloGameDims stepSolvePixeloGameCol width game
  where
    width = snd . snd . bounds $ pixeloGameGetBoard game

emptyGameBoard :: Int -> Int -> Array (Int, Int) PixeloTile
emptyGameBoard height width = listArray ((0, 0), (height - 1, width - 1))
      $ replicate (height * width) Unknown

stepSolvePixeloGame :: PixeloGame -> Maybe PixeloGame
stepSolvePixeloGame = stepSolvePixeloGameCols <=< stepSolvePixeloGameRows

-- | Solves the puzzle if it is solvable
solvePixeloGame :: PixeloGame -> Maybe PixeloGame
solvePixeloGame pixeloGame =   
  let 
    iteratedSols = iterate (>>= stepSolvePixeloGame) $ Just pixeloGame
    pairedSkewedSols = zip iteratedSols (tail iteratedSols)
    terminatingSols = takeWhile (\(sol0, sol1) -> sol0 /= sol1) pairedSkewedSols
  in
    snd . last $ terminatingSols
