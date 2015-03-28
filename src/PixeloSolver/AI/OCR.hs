{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{- module PixeloSolver.AI.OCR(
  PixeloBoard(..)
  , pixeloBoardGetTileWidth
  , pixeloBoardGetWidth
  , pixeloBoardGetHeight
  , screenshotToPixeloGame
  , findPixeloBoard
  ) where -}

module PixeloSolver.AI.OCR where

import Control.Applicative
import Control.Monad.Reader
import Data.List
import Data.Maybe
import Text.Parsec() -- in 3.1.3 instance Stream [tok] m tok is here
import Text.Parsec.Prim hiding (many, (<|>))
import Text.Parsec.Pos
import Text.Parsec.Combinator

import PixeloSolver.Data.Graphics
import PixeloSolver.Game.Nonogram
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (a:_) = Just a

-- | distance tolerance between board's white tile
distanceTolerance :: Int
distanceTolerance = 6

minimalTileLength :: Int
minimalTileLength = 15

emptySpaceTolerance :: Int
emptySpaceTolerance = 35

data NumberTolerances = NT {
  ntGetMaxNumberWidth :: Int
  , ntGetMaxInterDigitSpace :: Int
}

numberTolerance :: NumberTolerances
numberTolerance = NT 26 12

class BlackCheckable e where
  isBlack :: e -> Bool

instance BlackCheckable BW where
  isBlack Black = True
  isBlack _ = False

instance BlackCheckable RGB where
  isBlack = (== black)

data PixeloBoard = PixeloBoard {
  pixeloBoardGetRows :: [(Int, Int)]
  , pixeloBoardGetColumns :: [(Int, Int)]
} deriving (Eq, Show)

pixeloBoardGetWidth :: PixeloBoard -> Int
pixeloBoardGetWidth = length . pixeloBoardGetColumns

pixeloBoardGetHeight :: PixeloBoard -> Int
pixeloBoardGetHeight = length . pixeloBoardGetRows

pixeloBoardGetTileWidth :: PixeloBoard -> Int
pixeloBoardGetTileWidth pixeloBoard = rE - rB
  where (rB, rE) = head . pixeloBoardGetRows $ pixeloBoard

pixeloBoardGetFirstRowY :: PixeloBoard -> Int
pixeloBoardGetFirstRowY = fst . head . pixeloBoardGetRows

pixeloBoardGetFirstColX :: PixeloBoard -> Int
pixeloBoardGetFirstColX = fst . head . pixeloBoardGetColumns

-- | Extracts hint information from the screenshot and uses OCR to transform it
-- into a PixeloGame object
screenshotToPixeloGame :: (Map m r RGB) => m RGB -> PixeloBoard -> IO PixeloGame
screenshotToPixeloGame colorMap pixeloBoard =
  do
    rowHints <- sequence (map specPicsToHint rowHintPics)
    colHints <- sequence (map specPicsToHint colHintPics)
    let
      rowHints' = map (\l -> case l of { [] -> [0]; _ -> l; }) rowHints
      colHints' = map (\l -> case l of { [] -> [0]; _ -> l; }) colHints
    return $ PixeloGame (emptyGameBoard height width) rowHints' colHints'
  where
    columnHintsStrips = getColumnHints colorMap pixeloBoard -- :: [[m RGB]]
    colHintPics' = map
      (map (map (trimNonblack . snd) . splitBlackPatchesByColumn))
      columnHintsStrips
    rowHintPics' = map (mergeHints numberTolerance)
      $ getRowHints colorMap pixeloBoard

    colHintPics = map (map (map colorMapToBWMap)) colHintPics'
    rowHintPics = map (map (map colorMapToBWMap)) rowHintPics'
    (height, width) = (length rowHintPics, length colHintPics)

groupNeighbours :: (a -> a -> Bool) -> [a] -> [[a]]
groupNeighbours check as = groupNeighbours' [] check as
  where
    groupNeighbours' :: [a] -> (a -> a -> Bool) -> [a] -> [[a]]
    groupNeighbours' [] _ [] = []
    groupNeighbours' acc _ [] = [reverse acc]
    groupNeighbours' [] checkFun (x : xs) = groupNeighbours' [x] checkFun xs
    groupNeighbours' (accHead : acc) checkFun (x : xs) =
        if checkFun accHead x
        then groupNeighbours' (x : (accHead : acc)) checkFun xs
        else reverse (accHead : acc) : (groupNeighbours' [] checkFun (x : xs))

neighbourGroupToRanges :: [[(Int, a)]] -> [(Int, Int)]
neighbourGroupToRanges [] = []
neighbourGroupToRanges (ns : nss) = (fst . head $ ns, fst . last $ ns) :
    (neighbourGroupToRanges nss)

groupSimilarRanges :: Int -> [(Int, Int)] -> [[(Int, Int)]]
groupSimilarRanges tolerance = groupNeighbours
  (\(beg0, end0) (beg1, end1) ->
    (abs ((end1 - beg1) - (end0 - beg0)) <= tolerance) &&
    (beg1 - end0 - 1 <= tolerance))

findWhitePatches :: [(Int, RGB)] -> [(Int, Int)]
findWhitePatches row =
  let
    whites = filter (\(_, c) -> c == white) row
    neighbourRanges = neighbourGroupToRanges $ groupNeighbours
      (\(i0, _) (i1, _) -> i1 - i0 == 1)
      whites
  in
    neighbourRanges

getPixeloBoardRow :: [(Int, RGB)] -> Maybe [(Int, Int)]
getPixeloBoardRow row =
  let
    whitePatches = findWhitePatches row
    whitePatchesOfSimilarLength = groupSimilarRanges
        distanceTolerance
        (filter ((>= minimalTileLength) . (\(a, b) -> b - a + 1)) whitePatches)
    potentialRows = filter ((>= 5) . length) whitePatchesOfSimilarLength
  in
    safeHead potentialRows

findPixeloBoardRow :: [[(Int, RGB)]] -> Maybe [(Int, Int)]
findPixeloBoardRow = safeHead . catMaybes . map getPixeloBoardRow

findPixeloBoard :: Map m r RGB => m RGB -> Maybe PixeloBoard
findPixeloBoard colorMap = do
  let (height, width) = mapGetSize colorMap
  let rows = map rowAssocs $ map (\i -> mapGetRow i colorMap) (reverse [0..height])
  let columns = map rowAssocs $ map (\i -> mapGetColumn i colorMap) (reverse [0..width])
  boardColumn <- findPixeloBoardRow rows
  boardRow <- findPixeloBoardRow columns
  return $ PixeloBoard boardRow boardColumn

splitBlackPatchesByColumn :: (BlackCheckable e, Map m r e)
  => m e
  -> [((Int, Int), m e)]
splitBlackPatchesByColumn = splitBlackPatchesByColumn' 0

splitBlackPatchesByColumn' :: (BlackCheckable e, Map m r e)
  => Int
  -> m e
  -> [((Int, Int), m e)]
splitBlackPatchesByColumn' shift strip =
  case maybeBegOfFirstPatch of
    Just (begColumn, _) -> ((shift + begColumn, shift + endColumn),
      submap ((0, begColumn), (height, endColumn)) strip)
      : splitBlackPatchesByColumn'
        (shift + endColumn + 1)
        (submap ((0, endColumn + 1), (height, width)) strip)
    Nothing -> []
  where
    (height, width) = mapGetSize strip
    idxColumns = zip [0..width]
      (map (\i -> mapGetColumn i strip) [0..width])
    (maybeBegOfFirstPatch, columnsTail) =
      findFirstWithTail (any isBlack . rowElems . snd) idxColumns
    maybeEndOfFirstPatch = findFirst (not . any isBlack . rowElems . snd)
      columnsTail
    endColumn = fromMaybe width . fmap fst $ maybeEndOfFirstPatch

splitBlackPatchesByRow :: (BlackCheckable e, Map m r e)
  => m e
  -> [m e]
splitBlackPatchesByRow strip =
  case maybeBegOfFirstPatch of
    Just (begRow, _) -> submap ((begRow, 0), (endRow, width)) strip
      : splitBlackPatchesByRow
        (submap ((endRow + 1, 0), (height, width)) strip)
    Nothing -> []
  where
    (height, width) = mapGetSize strip
    idxRows = zip [0..height]
      (map (\i -> mapGetRow i strip) [0..height])
    (maybeBegOfFirstPatch, rowsTail) =
      findFirstWithTail (any isBlack . rowElems . snd) idxRows
    maybeEndOfFirstPatch = findFirst (not . any isBlack . rowElems . snd)
      rowsTail
    endRow = fromMaybe height . fmap fst $ maybeEndOfFirstPatch

findFirst :: (a -> Bool) -> [a] -> Maybe a
findFirst predicate as = safeHead . filter predicate $ as

findFirstWithTail :: (a -> Bool) -> [a] -> (Maybe a, [a])
findFirstWithTail _ [] = (Nothing, [])
findFirstWithTail predicate (a : as) =
  if predicate a
  then (Just a, as)
  else findFirstWithTail predicate as

findLast :: (a -> Bool) -> [a] -> Maybe a
findLast predicate as =
  case first of
    Nothing -> Nothing
    _ ->
      case second of
        Nothing -> first
        Just a -> findLast predicate (a : foundTail')

  where
    (first, foundTail) = findFirstWithTail predicate as
    (second, foundTail') = findFirstWithTail predicate foundTail

-- | trims all edge rows that do not contain a black pixel
trimNonblack :: (BlackCheckable e, Map m r e) => m e -> m e
trimNonblack bwMap =
    case maybeTrimmed of
      Just finalBWMap -> finalBWMap
      Nothing -> submap ((h + 1, w + 1), (h, w)) bwMap
    where
      (h, w) = mapGetSize bwMap
      rowsWithIdx = map (\i -> (i, mapGetRow i bwMap)) [0..h]
      colsWithIdx = map (\i -> (i, mapGetColumn i bwMap)) [0..w]
      firstRow = fmap fst
        . findFirst (\(_, c) -> any isBlack (rowElems c))
        $ rowsWithIdx
      lastRow = fmap fst
        . findLast (\(_, c) -> any isBlack (rowElems c))
        $ rowsWithIdx
      firstCol = fmap fst
        . findFirst (\(_, c) -> any isBlack (rowElems c))
        $ colsWithIdx
      lastCol = fmap fst
        . findLast (\(_, c) -> any isBlack (rowElems c))
        $ colsWithIdx
      maybeTrimmed = do
        fR <- firstRow
        lR <- lastRow
        fC <- firstCol
        lC <- lastCol
        return $ submap ((fR, fC), (lR, lC)) bwMap

getColumnHints :: (BlackCheckable e, Map m r e)
  => m e
  -> PixeloBoard
  -> [[m e]]
getColumnHints screenshot board =
  map (map trimNonblack . splitBlackPatchesByRow)
  . map (trimColumnHintsStrip emptySpaceTolerance)
  $ cutColumnHintsStrips screenshot board

getRowHints :: (BlackCheckable e, Map m r e)
  => m e
  -> PixeloBoard
  -> [[((Int, Int), m e)]]
getRowHints screenshot board =
  map (map (fmap trimNonblack) . splitBlackPatchesByColumn)
  . map (trimRowHintsStrip emptySpaceTolerance)
  $ cutRowHintsStrips screenshot board

-- | Given dimenstions and images of hints it merges hints that might represent
-- the same number based on NumberTolerances
mergeHints :: NumberTolerances -> [((Int, Int), a)] -> [[a]]
mergeHints _ [] = []
mergeHints _ (x : []) = [[snd x]]
mergeHints t (x : y : xs) =
  if joinedWidth <= ntGetMaxNumberWidth t
    && midDistances <= ntGetMaxInterDigitSpace t
  then [snd x, snd y] : mergeHints t xs
  else [snd x] : mergeHints t (y : xs)
  where
    (yBegFst, yEndFst) = fst $ x
    (yBegSnd, yEndSnd) = fst $ y
    joinedWidth = yEndSnd - yBegFst
    midDistances = (-)
      ((yBegSnd + yEndSnd) `div` 2)
      ((yBegFst + yEndFst) `div` 2)

hintPicsToInt :: [BWMap] -> IO Int
hintPicsToInt bwMaps =
  case recognizeNumber bwMaps of
    Just num -> return num
    _ -> do
        digits <- mapM (\bwMap -> do
          putStr . prettyPrintBWMap $ bwMap
          num <- getLine
          return $ read num) bwMaps
        return $ foldl (\a b -> 10 * a + b) 0 digits

specPicsToHint :: [[BWMap]] -> IO [Int]
specPicsToHint [] = return []
specPicsToHint (p : ps) = do
  spec <- hintPicsToInt p
  restOfHint <- specPicsToHint ps
  return (spec : restOfHint)

-- TODO Blog Above we have a IO dependence that pipes/conduit solves

-- Strip extraction functions

-- | Cut submaps of a screenshot which are aligned with board's columns and
-- start at the vertical beginning of the screenshot and end on first row.
cutColumnHintsStrips :: (Map m r e) => m e -> PixeloBoard -> [m e]
cutColumnHintsStrips m b = map (cutColumnHintsStrip m)
  . pixeloBoardGetColumns $ b
  where
    yEnd = pixeloBoardGetFirstRowY b
    cutColumnHintsStrip :: (Map m r e) => m e -> (Int, Int) -> m e
    cutColumnHintsStrip strip (xBeg, xEnd) = submap
      ((0, xBeg), (yEnd, xEnd))
      strip

-- | Cut submaps of a screenshot which are aligned with board's rows and start
-- at the horizontal beginning of the screenshot and end on first column.
cutRowHintsStrips :: (Map m r e) => m e -> PixeloBoard -> [m e]
cutRowHintsStrips m b = map (cutRowHintsStrip m)
  . pixeloBoardGetRows $ b
  where
    xEnd = pixeloBoardGetFirstColX b
    cutRowHintsStrip :: (Map m r e) => m e -> (Int, Int) -> m e
    cutRowHintsStrip strip (yBeg, yEnd) = submap
      ((yBeg, 0), (yEnd, xEnd))
      strip

-- | Trims column hints strip. Starting from bottom of the strip it searches for
-- N consecutive rows that do not have black pixels in them. Then it cuts the
-- top of the strip at that point. N is also called empty space tolerance.
trimColumnHintsStrip ::
  (BlackCheckable e, Map m r e)
  => Int -- ^ empty space tolerance.
  -> m e -- ^ hint strip
  -> m e
trimColumnHintsStrip t m = submap ((lastHintRow, 0), (height, width)) m
  where
    (height, width) = mapGetSize $ m
    lastHintRow = (height + 1 -)
      $ findLastHintDim mapGetRow t m (reverse [0..height])

-- | Row version of trimColumnHintsStrip
trimRowHintsStrip ::
  (BlackCheckable e, Map m r e)
  => Int -- ^ empty space tolerance.
  -> m e
  -> m e
trimRowHintsStrip t m = submap ((0, lastHintColumn), (height, width)) m
  where
    (height, width) = mapGetSize $ m
    lastHintColumn = (width + 1 -)
      $ findLastHintDim mapGetColumn t m (reverse [0..width])

hasRowBlack :: (BlackCheckable e, Row r e) => r e -> Bool
hasRowBlack = any (isBlack) . rowElems

-- Finds last row in a hint strip such that it contains a black pixel and up to
-- (tolerance + 1) next rows do not. Row here means either horizontal or
-- vertical row.
findLastHintDim ::
  (BlackCheckable e, Map m r e)
  => ((BlackCheckable e, Map m r e) => Int -> m e -> r e) -- ^ get row function
  -> Int -- ^ tolerance parameter
  -> m e -- ^ hint strip
  -> [Int] -- ^ row indexes to search over
  -> Int
findLastHintDim getDim tolerance m searchOrder =
  length
  . concat
  . takeWhile (\bs -> head bs == True || length bs <= tolerance)
  . group
  . map (\i -> hasRowBlack . getDim i $ m)
  $ searchOrder


-- Number OCR

type BlackGroups = [(Int, Int)]
type RowWidth = Int

type DigitRecognizer = ParsecT [BlackGroups] BlackGroups (Reader RowWidth)

blackGroupToken :: (BlackGroups -> Bool)
  -> DigitRecognizer ()
blackGroupToken predicate = tokenPrim show (\s _ _ -> incSourceColumn s 1)
  (\t -> if predicate t then return t else fail "")
  >>= putState

ellipse :: DigitRecognizer [()]
ellipse = many1 ellipseBeg >> many1 ellipseMid >> many1 ellipseEnd

ellipseBeg :: DigitRecognizer ()
ellipseBeg = do
  s <- getState
  width <- ask
  case s of 
    [] -> blackGroupToken (coveringMiddle width)
    [(xBeg, xEnd)] -> blackGroupToken 
      ((&&) <$> coveringMiddle width <*> predicate)
      where 
        predicate [(xBeg', xEnd')] = xBeg' <= xBeg 
          && xEnd' >= xEnd
        predicate _ = False
    _ -> fail ""
  where
    coveringMiddle width [(xBeg, xEnd)] = xEnd > (width `div` 2)
      && xBeg < (width `div` 2)
    coveringMiddle _ _ = False

ellipseMid :: DigitRecognizer ()
ellipseMid = do
  s <- getState
  case s of 
    [(xBeg, xEnd)] -> blackGroupToken predicate
      where 
        predicate [(x0Beg, _), (_, x1End)] = x0Beg <= xBeg 
          && x1End >= xEnd
        predicate _ = False
    [_, _] -> blackGroupToken ((== 2) . length)
    _ -> fail ""

ellipseEnd :: DigitRecognizer ()
ellipseEnd = do
  s <- getState
  width <- ask
  case s of 
    [(x0Beg, _), (_, x1End)] -> blackGroupToken 
      ((&&) <$> coveringMiddle width <*> predicate)
      where 
        predicate [(xBeg, xEnd)] = xBeg >= x0Beg && xEnd <= x1End
        predicate _ = False
    [(xBeg, xEnd)] -> blackGroupToken
      ((&&) <$> coveringMiddle width <*> predicate)
      where 
        predicate [(xBeg', xEnd')] = xBeg' >= xBeg 
          && xEnd' <= xEnd
        predicate _ = False
    _ -> fail ""
  where
    coveringMiddle width [(xBeg, xEnd)] = xEnd > (width `div` 2)
      && xBeg < (width `div` 2)
    coveringMiddle _ _ = False

leftEdge :: DigitRecognizer ()
leftEdge = blackGroupToken predicate
  where
    predicate [(xBeg, _)] = xBeg == 0
    predicate _ = False

rightEdge :: DigitRecognizer ()
rightEdge = do
  width <- ask
  blackGroupToken (predicate width)
  where
    predicate width [(_, xEnd)] = xEnd == width
    predicate _ _ = False

coveringBar :: DigitRecognizer ()
coveringBar = do
  width <- ask
  blackGroupToken (predicate width)
  where
    predicate width [(xBeg, xEnd)] = xBeg == 0 && xEnd == width
    predicate _ _ = False

zero :: DigitRecognizer Int
zero = ellipse >> eof >> return 0

one :: DigitRecognizer Int
one = many1 coveringBar >> eof >> return 1

two :: DigitRecognizer Int
two = many1 ellipseBeg 
  >> rightEdge 
  >> twoMiddle 
  >> many1 coveringBar 
  >> eof 
  >> return 2

twoMiddle :: DigitRecognizer [()]
twoMiddle = many1 $ do
  s <- getState
  case s of
    [(xBeg, xEnd)] -> blackGroupToken predicate
      where
        predicate [(xBeg', xEnd')] = xBeg' <= xBeg && xEnd' <= xEnd
        predicate _ = False
    _ -> fail ""

three :: DigitRecognizer Int
three = threeTwoForks >> threeThreeForks >> threeEnd >> eof >> return 3

threeTwoForks :: DigitRecognizer [()]
threeTwoForks = many $ blackGroupToken ((== 2) . length)

threeThreeForks :: DigitRecognizer [()]
threeThreeForks = many1 $ blackGroupToken ((== 3) . length)

threeEnd :: DigitRecognizer [()]
threeEnd = many1 $ blackGroupToken ((== 1) . length)

four :: DigitRecognizer Int
four = fourTwoForks >> many1 coveringBar >> fourBottom >> eof >> return 4

fourTwoForks :: DigitRecognizer [()]
fourTwoForks = many1 $ blackGroupToken predicate
  where
    predicate [(xBeg, _), _] = xBeg == 0
    predicate _ = False

fourBottom :: DigitRecognizer [()]
fourBottom = many1 $ do 
  width <- ask
  blackGroupToken (predicate width)
  where
    predicate width [(xBeg, _)] = xBeg > (width `div` 2)
    predicate _ _ = False

-- recognizeFive
five :: DigitRecognizer Int
five = many1 coveringBar 
  >> leftEdge 
  >> fiveMiddle 
  >> fiveBottom 
  >> eof 
  >> return 5

fiveMiddle :: DigitRecognizer [()]
fiveMiddle = many1 $ do
  s <- getState
  case s of
    [(xBeg, xEnd)] -> blackGroupToken predicate
      where
        predicate [(xBeg', xEnd')] = xBeg' >= xBeg && xEnd' >= xEnd
        predicate _ = False
    _ -> fail ""

fiveBottom :: DigitRecognizer [()]
fiveBottom = many1 $ do
  s <- getState
  case s of
    [(_, xEnd)] -> blackGroupToken predicate
      where
        predicate [(xBeg', xEnd')] = xBeg' == 0 && xEnd' <= xEnd
        predicate _ = False
    _ -> fail ""

six :: DigitRecognizer Int
six = many ellipseBeg >> many1 leftEdge >> many1 ellipseMid 
  >> many1 ellipseEnd 
  >> eof 
  >> return 6

nine :: DigitRecognizer Int
nine = ellipse >> many1 rightEdge >> many ellipseEnd >> eof >> return 9

seven :: DigitRecognizer Int
seven = many1 sevenTop >> many1 sevenBottom >> eof >> return 7

sevenTop :: DigitRecognizer ()
sevenTop = do
  s <- getState
  case s of
   [] -> blackGroupToken ((== 1) . length)
   [(xBeg, _)] -> blackGroupToken predicate
     where
       predicate [(xBeg', _)] = xBeg' <= xBeg
       predicate _ = False
   _ -> fail ""

sevenBottom :: DigitRecognizer ()
sevenBottom = do
  s <- getState
  case s of
   [(xBeg, xEnd)] -> blackGroupToken predicate
     where
       predicate [(xBeg', xEnd')] = xBeg' >= xBeg && xEnd' == xEnd
       predicate _ = False
   _ -> fail ""

eight :: DigitRecognizer Int
eight = ellipse 
  >> try (many ellipseBeg >> many1 ellipseMid) <|> many1 ellipseMid
  >> many1 ellipseEnd 
  >> eof 
  >> return 8

-- | Performan an OCR on a list of digits
recognizeNumber :: (BlackCheckable e, Map m r e) => [m e] -> Maybe Int
recognizeNumber images =
  fmap digitsToNumber . sequence
    . map ((<|>) <$> recognizeDigit <*> splitAndRecognizeDigits)
    $ images
  where
    digitsToNumber :: [Int] -> Int
    digitsToNumber = foldl (\a b -> 10 * a + b) 0
  
-- | Given image of a potential digit perform an OCR to recognize which one is
-- it.
recognizeDigit :: (BlackCheckable e, Map m r e) => m e -> Maybe Int
recognizeDigit bwMap = (run
    width 
    (choice . map try $ [zero, one, two, four, five, six, seven, eight, nine])
    rows)
  <|> (run height three columns)
  where 
    (height, width) = mapGetSize bwMap
    rows = map (\i -> getBlackGroups . mapGetRow i $ bwMap) [0..height]
    columns = map (\i -> getBlackGroups . mapGetColumn i $ bwMap) [0..width]
    run size parser = fromRight . ($ size) . runReader . runPT parser [] "Digit"
    fromRight (Right t) = return t
    fromRight (Left _) = fail ""

-- | Sometimes double digit number are have joined digits and are represented by
-- one picture. This method splits a picture and checks whether its constituents
-- are digits
splitAndRecognizeDigits :: (BlackCheckable e, Map m r e) => m e -> Maybe Int
splitAndRecognizeDigits image = do
  m <- recognizeDigit left
  n <- recognizeDigit right
  return $ m * 10 + n
  where 
    (height, width) = mapGetSize image
    tolerance = 2 -- We need to cut some middle to be sure that the digit does
                  -- not contain fragments from the other
    left = submap ((0, 0), (height, width `div` 2 - tolerance)) image
    right = submap ((0, width `div` 2 + tolerance), (height, width)) image

getBlackGroups :: (BlackCheckable e, Row r e) => r e -> [(Int, Int)]
getBlackGroups = getBlackGroups' . rowAssocs

getBlackGroups' :: (BlackCheckable e) => [(Int, e)] -> [(Int, Int)]
getBlackGroups' bws =
  map (\bs -> (fst . head $ bs, fst . last $ bs))
  . filter (isBlack . snd . head)
  . groupBy (\(_, c0) (_, c1) -> isBlack c0 == isBlack c1)
  $ bws
