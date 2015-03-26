{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
module PixeloSolver.AI.OCR(
  PixeloBoard(..)
  , pixeloBoardGetTileWidth
  , pixeloBoardGetWidth
  , pixeloBoardGetHeight
  , colorMapToPixeloGame
  , findPixeloBoard
  ) where
import Control.Applicative
import Data.Array
import Data.List
import Data.Maybe

import PixeloSolver.Data.Array.Extra
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
emptySpaceTolerance = 22

class BlackCheckable e where
  isBlack :: e -> Bool

instance BlackCheckable BW where
  isBlack Black = True
  isBlack _ = False

instance BlackCheckable RGB where
  isBlack = (== black)

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

data PixeloBoard = PixeloBoard {
  pixeloBoardGetRows :: [(Int, Int)],
  pixeloBoardGetColumns :: [(Int, Int)]
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

type ColHintStripe = BWMap
type RowHintStripe = BWMap

stripeLengthMultiplier :: Int
stripeLengthMultiplier = 6

getColHintStripes :: BWMap -> PixeloBoard -> [ColHintStripe]
getColHintStripes bwMap board =
  map (\(cB, cE) ->
      submap (((firstRowY - stripeLength), cB), (firstRowY, cE)) bwMap)
    colDims
  where
    firstRowY = pixeloBoardGetFirstRowY board
    stripeLength = stripeLengthMultiplier * (pixeloBoardGetTileWidth board)
    colDims = pixeloBoardGetColumns board

getRowHintStripes :: BWMap -> PixeloBoard -> [RowHintStripe]
getRowHintStripes bwMap board =
  map (\(rB, rE) ->
      submap ((rB, (firstColX - stripeLength)), (rE, firstColX)) bwMap)
    rowDims
  where
    firstColX = pixeloBoardGetFirstColX board
    stripeLength = stripeLengthMultiplier * (pixeloBoardGetTileWidth board)
    rowDims = pixeloBoardGetRows board

splitBlackPatchesByColumn :: (BlackCheckable e, Map m r e) 
  => m e
  -> [m e]
splitBlackPatchesByColumn stripe = 
  case maybeBegOfFirstPatch of
    Just (begColumn, _) -> submap ((0, begColumn), (height, endColumn)) stripe 
      : splitBlackPatchesByColumn 
        (submap ((0, endColumn + 1), (height, width)) stripe)
    Nothing -> []
  where
    (height, width) = mapGetSize stripe
    idxColumns = zip [0..height] 
      (map (\i -> mapGetColumn i stripe) [0..height])
    (maybeBegOfFirstPatch, columnsTail) = 
      findFirstWithTail (any isBlack . rowElems . snd) idxColumns
    maybeEndOfFirstPatch = findFirst (not . any isBlack . rowElems . snd)
      columnsTail
    endColumn = fromMaybe width . fmap fst $ maybeEndOfFirstPatch

splitBlackPatchesByRow :: (BlackCheckable e, Map m r e) 
  => m e
  -> [m e]
splitBlackPatchesByRow stripe = 
  case maybeBegOfFirstPatch of
    Just (begRow, _) -> submap ((begRow, 0), (endRow, width)) stripe 
      : splitBlackPatchesByRow 
        (submap ((endRow + 1, 0), (height, width)) stripe)
    Nothing -> []
  where
    (height, width) = mapGetSize stripe
    idxRows = zip [0..width] 
      (map (\i -> mapGetRow i stripe) [0..width])
    (maybeBegOfFirstPatch, tail) = 
      findFirstWithTail (any isBlack . rowElems . snd) idxRows
    maybeEndOfFirstPatch = findFirst (not . any isBlack . rowElems . snd) tail
    endRow = fromMaybe height . fmap fst $ maybeEndOfFirstPatch

splitBWMapByColumn :: BWMap -> [BWMap]
splitBWMapByColumn = splitBWMapByColumn' 0 0

splitBWMapByRow :: BWMap -> [BWMap]
splitBWMapByRow = splitBWMapByRow' 0 0

splitBWMapByColumn' :: Int -> Int -> BWMap -> [BWMap]
splitBWMapByColumn' first curCol bwMap = 
  if curCol < width
  then
    if any (== Black) (elems . getColumn curCol $ bwMap)
    then splitBWMapByColumn' first (curCol + 1) bwMap
    else 
      if first == curCol
      then splitBWMapByColumn' (curCol + 1) (curCol + 1) bwMap
      else (submap ((0, first), (height, curCol - 1)) bwMap) :
        (splitBWMapByColumn' (curCol + 1) (curCol + 1) bwMap)
  else 
    if first < curCol
    then [submap ((0, first), (height, curCol - 1)) bwMap]
    else []
  where
    (height, width) = snd . bounds $ bwMap

splitBWMapByRow' :: Int -> Int -> BWMap -> [BWMap]
splitBWMapByRow' first curRow bwMap =
  if curRow < height
  then
    if any (== Black) (elems . getRow curRow $ bwMap)
    then splitBWMapByRow' first (curRow + 1) bwMap
    else 
      if first == curRow
      then splitBWMapByRow' (curRow + 1) (curRow + 1) bwMap
      else (submap ((first, 0), (curRow - 1, width)) bwMap) :
        (splitBWMapByRow' (curRow + 1) (curRow + 1) bwMap)
  else 
    if first < curRow
    then [submap ((first, 0), (curRow - 1, width)) bwMap]
    else []
  where
    (height, width) = snd . bounds $ bwMap

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
  
splitRowHintStripe :: Int -> BWMap -> [BWMap]
splitRowHintStripe interDigitLeave bwMap = 
  case maybeFirstCol of
    Just firstCol -> submap ((0, firstCol), (h, lastCol)) bwMap :
      (splitRowHintStripe
        interDigitLeave
        (submap ((0, lastCol + 1), (h, w)) bwMap))
    Nothing -> []
  where
    (h, w) = snd . bounds $ bwMap
    colsWithIdx = map (\i -> (i, getColumn i bwMap)) [0..w]
    maybeFirstCol = fmap fst 
      . findFirst (\(_, col) -> any (== Black) (elems col))
      $ colsWithIdx
    findLastCol :: (Row r BW) => [(Int, r BW)] -> Int
    findLastCol cols = 
      if hasHeadAnyBlackPixel
      then
        if all isColumnWhite (take interDigitLeave . drop 1 . map snd $ cols)
        then fst . head $ cols
        else findLastCol (drop 1 cols)
      else findLastCol (drop 1 cols)
      where
        hasHeadAnyBlackPixel = any (== Black) (rowElems . snd . head $ cols)
        isColumnWhite :: (Row r BW) => r BW -> Bool
        isColumnWhite col = all (== White) (rowElems col)
    lastCol = findLastCol colsWithIdx


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

getColHintPics ::  BWMap -> PixeloBoard -> [[BWMap]]
getColHintPics bwMap board = 
  map (map trimNonblack . splitBWMapByRow) $ getColHintStripes bwMap board

getColumnHints ::  (BlackCheckable e, Map m r e) 
  => m e 
  -> PixeloBoard 
  -> [[m e]]
getColumnHints screenshot board = 
  map (map trimNonblack . splitBlackPatchesByRow) 
  . map (trimColumnHintsStripe  emptySpaceTolerance)
  $ cutColumnHintsStripes screenshot board

getRowHintPics ::  BWMap -> PixeloBoard -> [[BWMap]]
getRowHintPics bwMap board = 
  map (map trimNonblack . splitRowHintStripe 6) $ getRowHintStripes bwMap board

-- digit OCR
getBlackGroups :: (Row r BW) => r BW -> [(Int, Int)]
getBlackGroups = getBlackGroups' . rowAssocs

getBlackGroups' :: [(Int, BW)] -> [(Int, Int)]
getBlackGroups' bws = 
  map (\bs -> (fst . head $ bs, fst . last $ bs))
  . filter ((== Black) . snd . head)
  . groupBy (\(_, c0) (_, c1) -> c0 == c1) 
  $ bws

data RecognizeZeroState = RecognizeZeroStart
  | RecognizeZeroFirstEnd (Int, Int)
  | RecognizeZeroMiddle (Int, Int) (Int, Int)
  | RecognizeZeroFinalEnd (Int, Int)

recognizeZero :: BWMap -> Maybe Int
recognizeZero bwMap = 
  recognizeZero' RecognizeZeroStart (map (\i -> getRow i bwMap) [0..h])
  where
    h = fst . snd . bounds $ bwMap

recognizeZero' :: (Row r BW) => RecognizeZeroState -> [r BW] -> Maybe Int
recognizeZero' (RecognizeZeroFinalEnd _) [] = return 0

recognizeZero' _ [] = Nothing
recognizeZero' RecognizeZeroStart (r : rs) =
  case blackGroups of
    [b] ->
      if (fst b < middleW) && (snd b > middleW)
      then recognizeZero' (RecognizeZeroFirstEnd b) rs
      else Nothing
    _ -> Nothing
  where
    blackGroups = getBlackGroups r
    w = rowGetSize r
    middleW = w `div` 2

recognizeZero' (RecognizeZeroFirstEnd b) (r : rs) =
  case blackGroups of
    [b'] ->
      if ((fst b) >= (fst b')) && ((snd b) <= (snd b'))
      then recognizeZero' (RecognizeZeroFirstEnd b') rs
      else Nothing
    [b0, b1] ->
      if ((fst b) >= (fst b0)) && ((snd b) <= (snd b1))
      then recognizeZero' (RecognizeZeroMiddle b0 b1) rs
      else Nothing
    _ -> Nothing
  where
    blackGroups = getBlackGroups r

recognizeZero' (RecognizeZeroMiddle b0 b1) (r : rs) =
  case blackGroups of
    [b'] ->
      if ((fst b0) <= (fst b')) && ((snd b1) >= (snd b'))
        && (fst b' < middleW) && (snd b' > middleW)
      then recognizeZero' (RecognizeZeroFinalEnd b') rs
      else Nothing
    [b0', b1'] -> recognizeZero' (RecognizeZeroMiddle b0' b1') rs
    _ -> Nothing
  where
    blackGroups = getBlackGroups r
    w = rowGetSize r
    middleW = w `div` 2

recognizeZero' (RecognizeZeroFinalEnd b) (r : rs) =
  case blackGroups of
    [b'] ->
      if ((fst b) <= (fst b')) && ((snd b) >= (snd b'))
        && (fst b' < middleW) && (snd b' > middleW)
      then recognizeZero' (RecognizeZeroFinalEnd b') rs
      else Nothing
    _ -> Nothing
  where
    blackGroups = getBlackGroups r
    w = rowGetSize r
    middleW = w `div` 2

recognizeOne :: BWMap -> Maybe Int
recognizeOne bwMap =
  if all (== Black) (elems bwMap)
  then Just 1
  else Nothing

-- recognizeTwo

data RecognizeTwoState = RecognizeTwoStart
  | RecognizeTwoTop (Int, Int)
  | RecognizeTwoMiddle (Int, Int)
  | RecognizeTwoBottom (Int, Int)

recognizeTwo :: BWMap -> Maybe Int
recognizeTwo bwMap =
  recognizeTwo' RecognizeTwoStart (map (\i -> getRow i bwMap) [0..h])
  where
    h = fst . snd . bounds $ bwMap

recognizeTwo' :: (Row r BW) => RecognizeTwoState -> [r BW] -> Maybe Int

recognizeTwo' (RecognizeTwoBottom _) [] = return 2
recognizeTwo' _ [] = Nothing

recognizeTwo' RecognizeTwoStart (r : rs) =
  case blackGroups of
    [b] -> recognizeTwo' (RecognizeTwoTop b) rs
    _ -> Nothing
  where
    blackGroups = getBlackGroups r

recognizeTwo' (RecognizeTwoTop b) (r : rs) =
  case blackGroups of
    [b'] -> 
      if ((fst b) == (fst b')) && ((snd b) <= (snd b'))
      then recognizeTwo' (RecognizeTwoTop b') rs
      else
        if ((fst b) < (fst b')) && ((snd b) == (snd b'))
        then recognizeTwo' (RecognizeTwoMiddle b') rs
        else Nothing
    _ -> Nothing
  where
    blackGroups = getBlackGroups r

recognizeTwo' (RecognizeTwoMiddle b) (r : rs) =
  case blackGroups of
    [b'] -> 
      if ((fst b) >= (fst b')) && ((snd b) >= (snd b'))
      then recognizeTwo' (RecognizeTwoMiddle b') rs
      else
        if ((fst b) == (fst b')) && ((snd b) <= (snd b'))
        then recognizeTwo' (RecognizeTwoBottom b') rs
        else Nothing
    _ -> Nothing
  where
    blackGroups = getBlackGroups r

recognizeTwo' (RecognizeTwoBottom b) (r : rs) =
  case blackGroups of
    [b'] -> 
      if ((fst b) == (fst b')) && ((snd b) <= (snd b'))
      then recognizeTwo' (RecognizeTwoBottom b') rs
      else Nothing
    _ -> Nothing
  where
    blackGroups = getBlackGroups r

-- recognizeThree

data RecognizeThreeState = RecognizeThreeStart
  | RecognizeThreeTwoBounds (Int, Int) (Int, Int)
  | RecognizeThreeThreeBounds (Int, Int) (Int, Int) (Int, Int)
  | RecognizeThreeRightEnd (Int, Int)

recognizeThree :: BWMap -> Maybe Int
recognizeThree bwMap = 
  recognizeThree' RecognizeThreeStart (map (\i -> getColumn i bwMap) [0..w])
  where
    w = snd . snd . bounds $ bwMap

recognizeThree' :: (Row r BW) => RecognizeThreeState -> [r BW] -> Maybe Int

recognizeThree' (RecognizeThreeRightEnd _) [] = Just 3

recognizeThree' _ [] = Nothing

recognizeThree' RecognizeThreeStart (r : rs) = 
  case blackGroups of
    [b0, b1] -> 
      if (fst b0 == 0) && (snd b1 == h)
      then recognizeThree' (RecognizeThreeTwoBounds b0 b1) rs
      else Nothing
    [b0, b1, b2] ->
        if (fst b0 == 0) && (snd b2 == h)
        then recognizeThree' (RecognizeThreeThreeBounds b0 b1 b2) rs
        else Nothing
    _ -> Nothing
  where
    blackGroups = getBlackGroups r
    h = rowGetSize r

recognizeThree' (RecognizeThreeTwoBounds b0 b1) (r : rs) = 
  case blackGroups of
    [b0', b1'] -> 
      if (b0 == b0') && (b1 == b1') 
      then recognizeThree' (RecognizeThreeTwoBounds b0 b1) rs
      else Nothing
    [b0', b1', b2'] ->
      if (b0 == b0') && (b1 == b2')
      then recognizeThree' (RecognizeThreeThreeBounds b0' b1' b2') rs
      else Nothing
    _ -> Nothing
  where
    blackGroups = getBlackGroups r

recognizeThree' (RecognizeThreeThreeBounds b0 b1 b2) (r : rs) = 
  case blackGroups of
    [b] -> 
      if ((fst b) == (fst b0)) && ((snd b) == (snd b2))
      then recognizeThree' (RecognizeThreeRightEnd b) rs
      else Nothing
    [b0', b1', b2'] ->
      if (b0 == b0') && (b1 == b1') && (b2 == b2')
      then recognizeThree' (RecognizeThreeThreeBounds b0' b1' b2') rs
      else Nothing
    _ -> Nothing
  where
    blackGroups = getBlackGroups r

recognizeThree' (RecognizeThreeRightEnd b) (r : rs) = 
  case blackGroups of
    [b'] -> 
      if ((fst b) <= (fst b')) && ((snd b) >= (snd b'))
      then recognizeThree' (RecognizeThreeRightEnd b') rs
      else Nothing
    _ -> Nothing
  where
    blackGroups = getBlackGroups r

-- recognizeFour

data RecognizeFourState = RecognizeFourStart
  | RecognizeFourForks (Int, Int) (Int, Int)
  | RecognizeFourMiddleLine (Int, Int)
  | RecognizeFourBase (Int, Int)

recognizeFour :: BWMap -> Maybe Int
recognizeFour bwMap = recognizeFour' RecognizeFourStart
  (map (\i -> getRow i bwMap) [0..h])
  where
    h = fst . snd . bounds $ bwMap

recognizeFour' :: (Row r BW) => RecognizeFourState -> [r BW] -> Maybe Int

recognizeFour' (RecognizeFourBase _) [] = return 4
recognizeFour' _ [] = Nothing

recognizeFour' RecognizeFourStart (r : rs) =
  case blackGroups of
    [b0, b1] -> recognizeFour' (RecognizeFourForks b0 b1) rs
    _ -> Nothing
  where
    blackGroups = getBlackGroups r

recognizeFour' (RecognizeFourForks b0 b1) (r : rs) =
  case blackGroups of
    [b] ->
      if ((fst b) == (fst b0)) && ((snd b) >= (snd b1))
      then recognizeFour' (RecognizeFourMiddleLine b) rs
      else Nothing
    [b0', b1'] ->
      if (b0 == b0') && (b1 == b1')
      then recognizeFour' (RecognizeFourForks b0 b1) rs
      else Nothing
    _ -> Nothing
  where
    blackGroups = getBlackGroups r

recognizeFour' (RecognizeFourMiddleLine b) (r : rs) =
  case blackGroups of
    [b'] ->
      if b == b'
      then recognizeFour' (RecognizeFourMiddleLine b) rs
      else
        if ((fst b) < (fst b')) && ((snd b) >= (snd b'))
        then recognizeFour' (RecognizeFourBase b') rs
        else Nothing 
    _ -> Nothing
  where
    blackGroups = getBlackGroups r

recognizeFour' (RecognizeFourBase b) (r : rs) =
  case blackGroups of
    [b'] ->
      if b == b'
      then recognizeFour' (RecognizeFourBase b) rs
      else Nothing
    _ -> Nothing
  where
    blackGroups = getBlackGroups r

-- recognizeFive

data RecognizeFiveState = RecognizeFiveStart
  | RecognizeFiveTop (Int, Int)
  | RecognizeFiveMiddle (Int, Int)
  | RecognizeFiveBottom (Int, Int)

recognizeFive :: BWMap -> Maybe Int
recognizeFive bwMap =
  recognizeFive' RecognizeFiveStart (map (\i -> getRow i bwMap) [0..h])
  where
    h = fst . snd . bounds $ bwMap

recognizeFive' :: (Row r BW) => RecognizeFiveState -> [r BW] -> Maybe Int

recognizeFive' (RecognizeFiveBottom _) [] = return 5
recognizeFive' _ [] = Nothing

recognizeFive' RecognizeFiveStart (r : rs) =
  case blackGroups of
    [b] -> recognizeFive' (RecognizeFiveTop b) rs
    _ -> Nothing
  where
    blackGroups = getBlackGroups r

recognizeFive' (RecognizeFiveTop b) (r : rs) =
  case blackGroups of
    [b'] -> 
      if ((fst b) >= (fst b')) && ((snd b) == (snd b'))
      then recognizeFive' (RecognizeFiveTop b') rs
      else
        if ((fst b) == (fst b')) && ((snd b) > (snd b'))
        then recognizeFive' (RecognizeFiveMiddle b') rs
        else Nothing
    _ -> Nothing
  where
    blackGroups = getBlackGroups r

recognizeFive' (RecognizeFiveMiddle b) (r : rs) =
  case blackGroups of
    [b'] -> 
      if ((fst b) <= (fst b')) && ((snd b) <= (snd b'))
      then recognizeFive' (RecognizeFiveMiddle b') rs
      else
        if ((fst b) >= (fst b')) && ((snd b) == (snd b'))
        then recognizeFive' (RecognizeFiveBottom b') rs
        else Nothing
    _ -> Nothing
  where
    blackGroups = getBlackGroups r

recognizeFive' (RecognizeFiveBottom b) (r : rs) =
  case blackGroups of
    [b'] -> 
      if ((fst b) == (fst b')) && ((snd b) >= (snd b'))
      then recognizeFive' (RecognizeFiveBottom b') rs
      else Nothing
    _ -> Nothing
  where
    blackGroups = getBlackGroups r

-- | A hack which assumes all other numbers except for 6 and 9 have been
-- recognized
recognizeSixOrNine :: BWMap -> Maybe Int
recognizeSixOrNine bwMap =
  if leftEdgeSize < rightEdgeSize
  then return 9
  else return 6
  where 
    w = snd . snd . bounds $ bwMap
    leftEdgeSize = length . filter (== Black) . elems . getColumn 0 $ bwMap
    rightEdgeSize = length . filter (== Black) . elems . getColumn w $ bwMap

-- recognizeSeven

data RecognizeSevenState = RecognizeSevenStart
  | RecognizeSevenTop (Int, Int)
  | RecognizeSevenBottom (Int, Int)

recognizeSeven :: BWMap -> Maybe Int
recognizeSeven bwMap =
  recognizeSeven' RecognizeSevenStart (map (\i -> getRow i bwMap) [0..h])
  where
    h = fst . snd . bounds $ bwMap

recognizeSeven' :: (Row r BW) => RecognizeSevenState -> [r BW] -> Maybe Int
recognizeSeven' (RecognizeSevenBottom _) [] = return 7
recognizeSeven' _ [] = Nothing

recognizeSeven' RecognizeSevenStart (r : rs) =
  case blackGroups of
    [b] -> recognizeSeven' (RecognizeSevenTop b) rs
    _ -> Nothing
  where
    blackGroups = getBlackGroups r

recognizeSeven' (RecognizeSevenTop b) (r : rs) =
  case blackGroups of
    [b'] ->
      if b == b'
      then recognizeSeven' (RecognizeSevenTop b) rs
      else
        if ((fst b) < (fst b'))
        then recognizeSeven' (RecognizeSevenBottom b') rs
        else Nothing
    _ -> Nothing
  where
    blackGroups = getBlackGroups r

recognizeSeven' (RecognizeSevenBottom b) (r : rs) =
  case blackGroups of
    [b'] ->
      if b == b'
      then recognizeSeven' (RecognizeSevenBottom b) rs
      else Nothing
    _ -> Nothing
  where
    blackGroups = getBlackGroups r

-- recognizeEight

recognizeEight :: BWMap -> Maybe Int
recognizeEight bwMap =
  if (isJust (recognizeZero (submap ((0, 0), (h `div` 2, w)) bwMap)))
    && (isJust (recognizeZero (submap ((h `div` 2 + 1, 0), (h, w)) bwMap)))
  then Just 8
  else Nothing
  where
    (h, w) = snd . bounds $ bwMap

recognizeDigitFunctions :: [BWMap -> Maybe Int]
recognizeDigitFunctions = [
  recognizeZero,
  recognizeOne,
  recognizeTwo,
  recognizeThree,
  recognizeFour,
  recognizeFive,
  recognizeSeven,
  recognizeEight,
  recognizeSixOrNine]

recognizeDigit :: BWMap -> Maybe Int
recognizeDigit bwMap = safeHead
  . catMaybes 
  $ recognizeDigitFunctions <*> (pure bwMap)

recognizeNumber :: BWMap -> Maybe Int
recognizeNumber bwMap =
  fmap digitsToNumber . sequence . map recognizeDigit $ bwMaps
  where
    bwMaps = splitBWMapByColumn bwMap
    digitsToNumber :: [Int] -> Int
    digitsToNumber = foldl (\a b -> 10 * a + b) 0

-- Mapper from ColorMap to PixeloGame solver
--
specPicToHint :: BWMap -> IO Int
specPicToHint bwMap = 
  case recognizeNumber bwMap of
    Just num -> return num
    _ -> do
      putStr . prettyPrintBWMap $ bwMap
      num <- getLine
      return $ read num


specPicsToHint :: [BWMap] -> IO [Int]
specPicsToHint [] = return []
specPicsToHint (p : ps) = do
  spec <- specPicToHint p
  restOfHint <- specPicsToHint ps
  return (spec : restOfHint)

-- TODO Blog Above we have a IO dependence that pipes/conduit solves

-- | Extracts hint information from the screenshot and uses OCR to transform it
-- into a PixeloGame object
colorMapToPixeloGame :: (Map m r RGB) => m RGB -> PixeloBoard -> IO PixeloGame
colorMapToPixeloGame colorMap pixeloBoard =
  do
    rowHints <- sequence (map specPicsToHint rowHintPics)
    colHints <- sequence (map specPicsToHint colHintPics)
    let
      rowHints' = map (\l -> case l of { [] -> [0]; _ -> l; }) rowHints
      colHints' = map (\l -> case l of { [] -> [0]; _ -> l; }) colHints
    return $ PixeloGame (emptyGameBoard height width) rowHints' colHints'
  where
    bwMap = colorMapToBWMap colorMap
    colHintPics = getColHintPics bwMap pixeloBoard
    rowHintPics = getRowHintPics bwMap pixeloBoard
    (height, width) = (length rowHintPics, length colHintPics)

-- Stripe extraction functions

-- | Cut submaps of a screenshot which are aligned with board's columns and
-- start at the vertical beginning of the screenshot and end on first row.
cutColumnHintsStripes :: (Map m r e) => m e -> PixeloBoard -> [m e]
cutColumnHintsStripes m b = map (cutColumnHintsStripe m) 
  . pixeloBoardGetColumns $ b
  where
    yEnd = pixeloBoardGetFirstRowY b
    cutColumnHintsStripe :: (Map m r e) => m e -> (Int, Int) -> m e
    cutColumnHintsStripe stripe (xBeg, xEnd) = submap 
      ((0, xBeg), (yEnd, xEnd)) 
      stripe

-- | Cut submaps of a screenshot which are aligned with board's rows and start
-- at the horizontal beginning of the screenshot and end on first column.
cutRowHintsStripes :: (Map m r e) => m e -> PixeloBoard -> [m e]
cutRowHintsStripes m b = map (cutRowHintsStripe m)
  . pixeloBoardGetRows $ b
  where
    xEnd = pixeloBoardGetFirstColX b
    cutRowHintsStripe :: (Map m r e) => m e -> (Int, Int) -> m e
    cutRowHintsStripe stripe (yBeg, yEnd) = submap 
      ((yBeg, 0), (yEnd, xEnd)) 
      stripe


-- | Trims column hints strip. Starting from bottom of the strip it searches for
-- N consecutive rows that do not have black pixels in them. Then it cuts the
-- top of the strip at that point. N is also called empty space tolerance.
trimColumnHintsStripe ::
  (BlackCheckable e, Map m r e)
  => Int -- ^ empty space tolerance. 
  -> m e
  -> m e
trimColumnHintsStripe t m = submap ((lastHintRow, 0), (height, width)) m
  where
    (height, width) = mapGetSize $ m
    lastHintRow = (height + 1 -) 
      $ findLastHintDim mapGetRow t m (reverse [0..height])

-- | Row version of trimColumnHintsStripe
trimRowHintsStripe ::
  (BlackCheckable e, Map m r e)
  => Int -- ^ empty space tolerance. 
  -> m e
  -> m e
trimRowHintsStripe t m = submap ((0, lastHintColumn), (height, width)) m
  where
    (height, width) = mapGetSize $ m
    lastHintColumn = (height + 1 -)
      $ findLastHintDim mapGetColumn t m (reverse [0..width])

hasRowBlack :: (BlackCheckable e, Row r e) => r e -> Bool
hasRowBlack = any (isBlack) . rowElems

findLastHintDim :: 
  (BlackCheckable e, Map m r e) 
  => ((BlackCheckable e, Map m r e) => Int -> m e -> r e)
  -> Int
  -> m e
  -> [Int]
  -> Int
findLastHintDim getDim tolerance m searchOrder =
  length 
  . concat 
  . takeWhile (\bs -> head bs == True || length bs <= tolerance) 
  . group 
  . map (\i -> hasRowBlack . getDim i $ m) 
  $ searchOrder
