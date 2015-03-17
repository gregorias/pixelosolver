{-# LANGUAGE ScopedTypeVariables #-}
-- Fix solver crashing on wronly defined game
-- Make definition of constants explicit, especially the interDigitSpace.
-- Stripe length should be calculated more smartly, or by cutting it.
--Fix crashses when taking screenshot without board on it.
-- Fix stack overflow
-- Optimize, especially board finding
-- Finish recognition algorithm for 6 and 9
-- Fix recognition of double digits like 20 which do not have space in between.
--
--ghc --make Main -package-db=.cabal-sandbox/x86_64-linux-ghc-7.6.3-packages.conf.d/ -rtsopts
module Main where
import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.Array
import Data.Array.IArray(amap)
import Data.Array.MArray
import Data.List
import Data.Maybe
import Data.Word
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Screen
import System.Environment


import qualified Graphics.X11.Xlib as X
import Graphics.X11.XTest

import qualified Graphics.UI.WX as WX


safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (a:_) = Just a

-- Map manipulation

type RGB = (Word8, Word8, Word8)

getRed :: RGB -> Word8
getRed (r, _, _) = r

getGreen :: RGB -> Word8
getGreen (_, g, _) = g

getBlue :: RGB -> Word8
getBlue (_, _, b) = b

white :: RGB
white = (255, 255, 255)

black :: RGB
black = (0, 0, 0)

gridGrey :: RGB
gridGrey = (188, 188, 188)

gridRed :: RGB
gridRed = (238, 116, 116)

data BW = Black | White deriving (Eq, Show)

type Map = Array (Int, Int)
type Row = Array Int
type ColorMap = Array (Int, Int) RGB

mapGetWidth :: Map e -> Int
mapGetWidth = snd . snd . bounds

mapGetHeight :: Map e -> Int
mapGetHeight = fst . snd . bounds

type BWMap = Array (Int, Int) BW

colorMapToBWMap :: ColorMap -> BWMap
colorMapToBWMap = amap (\c -> if c == black then Black else White)

getRow :: Int -> Map e -> Row e
getRow row map = ixmap (0, width) (\i -> (row, i)) map
  where width = snd . snd . bounds $ map

setRow :: Int -> Row e -> Map e -> Map e
setRow rowId row inputMap =
  inputMap // (map (\(x, value) -> ((rowId, x), value)) (assocs row))

getColumn :: Int -> Map e -> Row e
getColumn column map = ixmap (0, height) (\i -> (i, column)) map
  where height = fst . snd . bounds $ map

setColumn colId column inputMap =
  inputMap // (map (\(y, value) -> ((y, colId), value)) (assocs column))

submap :: ((Int, Int), (Int, Int)) -> Map e -> Map e
submap ((begY, begX), (endY, endX)) =
  ixmap ((0, 0), (height, width)) (\(y, x) -> (y + begY, x + begX))
  where
    (height, width) = (endY - begY, endX - begX)

prettyPrintBW Black = '#'
prettyPrintBW White = '_'

prettyPrintBWMapRow :: Array Int BW -> String
prettyPrintBWMapRow bwMapRow = 
  (map prettyPrintBW (elems bwMapRow)) ++ ['\n']

prettyPrintBWMap :: BWMap -> String
prettyPrintBWMap bwMap = 
  concat $ map (\i -> prettyPrintBWMapRow $ getRow i bwMap) [0..height]
  where
    height = mapGetHeight bwMap

-- Map recognition

-- | distance tolerance between board's white tile
distanceTolerance = 6
minimalTileLength = 15

groupNeighbours :: (a -> a -> Bool) -> [a] -> [[a]]
groupNeighbours check as = groupNeighbours' [] check as
  where
    groupNeighbours' :: [a] -> (a -> a -> Bool) -> [a] -> [[a]]
    groupNeighbours' [] check [] = []
    groupNeighbours' acc check [] = [reverse acc]
    groupNeighbours' [] check (a:as) = groupNeighbours' [a] check as
    groupNeighbours' (last:acc) check (a:as) = 
        if check last a
        then groupNeighbours' (a:(last:acc)) check as
        else reverse (last:acc) : (groupNeighbours' [] check (a:as))

neighbourGroupToRanges :: [[(Int, a)]] -> [(Int, Int)]
neighbourGroupToRanges [] = []
neighbourGroupToRanges (ns:nss) = (fst . head $ ns, fst . last $ ns) :
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
    whites = filter (\(i, c) -> c == white) row
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

findPixeloBoard :: ColorMap -> Maybe PixeloBoard
findPixeloBoard colorMap = do
  let (height, width) = snd . bounds $ colorMap
  let rows = map assocs $ map (\i -> getRow i colorMap) (reverse [0..height])
  let columns = map assocs $ map (\i -> getColumn i colorMap) (reverse [0..width])
  boardColumn <- findPixeloBoardRow rows
  boardRow <- findPixeloBoardRow columns
  return $ PixeloBoard boardRow boardColumn

type ColSpecStripe = BWMap
type RowSpecStripe = BWMap

stripeLengthMultiplier = 6

getColSpecStripes :: BWMap -> PixeloBoard -> [ColSpecStripe]
getColSpecStripes bwMap board =
  map (\(cB, cE) ->
      submap (((firstRowY - stripeLength), cB), (firstRowY, cE)) bwMap)
    colDims
  where
    firstRowY = pixeloBoardGetFirstRowY board
    stripeLength = stripeLengthMultiplier * (pixeloBoardGetTileWidth board)
    colDims = pixeloBoardGetColumns board

getRowSpecStripes :: BWMap -> PixeloBoard -> [RowSpecStripe]
getRowSpecStripes bwMap board =
  map (\(rB, rE) ->
      submap ((rB, (firstColX - stripeLength)), (rE, firstColX)) bwMap)
    rowDims
  where
    firstColX = pixeloBoardGetFirstColX board
    stripeLength = stripeLengthMultiplier * (pixeloBoardGetTileWidth board)
    rowDims = pixeloBoardGetRows board

splitBWMapByColumn :: BWMap -> [BWMap]
splitBWMapByColumn = splitBWMapByColumn' 0 0

splitBWMapByRow :: BWMap -> [BWMap]
splitBWMapByRow = splitBWMapByRow' 0 0

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
findFirst  predicate as = safeHead . filter predicate $ as

findFirstWithTail :: (a -> Bool) -> [a] -> (Maybe a, [a])
findFirstWithTail predicate [] = (Nothing, [])
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
        Just a -> findLast predicate (a : tail')
      
  where
    (first, tail) = findFirstWithTail predicate as
    (second, tail') = findFirstWithTail predicate tail
  
splitRowSpecStripe :: Int -> BWMap -> [BWMap]
splitRowSpecStripe interDigitLeave bwMap = 
  case maybeFirstCol of
    Just firstCol -> submap ((0, firstCol), (h, lastCol)) bwMap :
      (splitRowSpecStripe
        interDigitLeave
        (submap ((0, lastCol + 1), (h, w)) bwMap))
    Nothing -> []
  where
    (h, w) = snd . bounds $ bwMap
    colsWithIdx = map (\i -> (i, getColumn i bwMap)) [0..w]
    maybeFirstCol = fmap fst 
      . findFirst (\(i, col) -> any (== Black) (elems col))
      $ colsWithIdx
    Just firstCol = maybeFirstCol
    findLastCol :: [(Int, Row BW)] -> Int
    findLastCol cols = 
      if hasHeadAnyBlackPixel
      then
        if all isColumnWhite (take interDigitLeave . drop 1 . map snd $ cols)
        then fst . head $ cols
        else findLastCol (drop 1 cols)
      else findLastCol (drop 1 cols)
      where
        hasHeadAnyBlackPixel = any (== Black) (elems . snd . head $ cols)
        isColumnWhite col = all (== White) (elems col)
    lastCol = findLastCol colsWithIdx


trimBWMap :: BWMap -> BWMap
trimBWMap bwMap = 
    case maybeTrimmed of
      Just finalBWMap -> finalBWMap
      Nothing -> array ((0, 0), (-1, -1)) []
    where 
      (h, w) = snd . bounds $ bwMap
      rowsWithIdx = map (\i -> (i, getRow i bwMap)) [0..h]
      colsWithIdx = map (\i -> (i, getColumn i bwMap)) [0..w]
      firstRow = fmap fst 
        . findFirst (\(i, c) -> any (== Black) (elems c))
        $ rowsWithIdx
      lastRow = fmap fst 
        . findLast (\(i, c) -> any (== Black) (elems c))
        $ rowsWithIdx
      firstCol = fmap fst 
        . findFirst (\(i, c) -> any (== Black) (elems c))
        $ colsWithIdx
      lastCol = fmap fst 
        . findLast (\(i, c) -> any (== Black) (elems c))
        $ colsWithIdx
      maybeTrimmed = do 
        fR <- firstRow
        lR <- lastRow
        fC <- firstCol
        lC <- lastCol
        return $ submap ((fR, fC), (lR, lC)) bwMap

getColSpecPics ::  BWMap -> PixeloBoard -> [[BWMap]]
getColSpecPics bwMap board = 
  map (map trimBWMap . splitBWMapByRow) $ getColSpecStripes bwMap board

getRowSpecPics ::  BWMap -> PixeloBoard -> [[BWMap]]
getRowSpecPics bwMap board = 
  map (map trimBWMap . splitRowSpecStripe 6) $ getRowSpecStripes bwMap board

-- digit OCR
getBlackGroups :: Row BW -> [(Int, Int)]
getBlackGroups = getBlackGroups' . assocs

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

recognizeZero' :: RecognizeZeroState -> [Row BW] -> Maybe Int
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
    w = snd . bounds $ r
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
    w = snd . bounds $ r
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
    w = snd . bounds $ r
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

recognizeTwo' :: RecognizeTwoState -> [Row BW] -> Maybe Int

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

recognizeThree' :: RecognizeThreeState -> [Row BW] -> Maybe Int

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
    h = snd . bounds $ r

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

recognizeFour' :: RecognizeFourState -> [Row BW]  -> Maybe Int

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

recognizeFive' :: RecognizeFiveState -> [Row BW] -> Maybe Int

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

recognizeSeven' :: RecognizeSevenState -> [Row BW] -> Maybe Int
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
specPicToSpec :: BWMap -> IO Int
specPicToSpec bwMap = 
  case recognizeNumber bwMap of
    Just num -> return num
    _ -> do
      putStr . prettyPrintBWMap $ bwMap
      num <- getLine
      return $ read num


specPicsToSpec :: [BWMap] -> IO [Int]
specPicsToSpec [] = return []
specPicsToSpec (p : ps) = do
  spec <- specPicToSpec p
  restOfSpec <- specPicsToSpec ps
  return (spec : restOfSpec)

-- TODO Blog Above we have a IO dependence that pipes/conduit solves

colorMapToPixeloGame :: ColorMap -> PixeloBoard -> IO PixeloGame
colorMapToPixeloGame colorMap pixeloBoard =
  do
    rowSpecs <- sequence (map specPicsToSpec rowSpecPics :: [IO [Int]]) :: IO [[Int]]
    colSpecs <- sequence (map specPicsToSpec colSpecPics)
    return $ PixeloGame emptyGameBoard rowSpecs colSpecs 
  where
    bwMap = colorMapToBWMap colorMap
    colSpecPics = getColSpecPics bwMap pixeloBoard
    rowSpecPics = getRowSpecPics bwMap pixeloBoard
    (height, width) = (length rowSpecPics, length colSpecPics)
    emptyGameBoard = listArray ((0, 0), (height - 1, width - 1))
      (take (height * width) $ repeat Unknown)

-- Pixelo game solver

data PixeloTile = Done PixeloTileFill | Unknown deriving (Eq, Show)
data PixeloTileFill = Empty | Full deriving (Eq, Show)

data PixeloGame = PixeloGame (Array (Int, Int) PixeloTile) [[Int]] [[Int]]
  deriving (Eq, Show)

pixeloGameGetBoard (PixeloGame board _ _) = board
pixeloGameGetRowSpecs (PixeloGame _ rowSpecs _) = rowSpecs
pixeloGameGetColSpecs (PixeloGame _ _ colSpecs) = colSpecs

prettyPrintTile (Done Empty) = '_'
prettyPrintTile (Done Full) = '#'
prettyPrintTile Unknown = '?'

prettyPrintBoard :: Array (Int, Int) PixeloTile -> String
prettyPrintBoard board = concat 
  $ map (\i -> (map prettyPrintTile $ elems (getRow i board)) ++ ['\n']) [0..height]
  where
    height = snd . snd . bounds $ board 

generateSolutions :: [Int] -> [PixeloTile] -> [[PixeloTileFill]]
generateSolutions ss cs = generateSolutions' False 0 ss cs

--generateSolutions' isStreak curStreak restOfSpec constraints
generateSolutions' :: Bool -> Int -> [Int] -> [PixeloTile] -> [[PixeloTileFill]]
generateSolutions' False _ [] [] = [[]]
generateSolutions' False _ [] (c : cs) =
  case c of
    Done Full -> []
    _ -> do 
      sol <- generateSolutions' False 0 [] cs
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

stepSolvePixeloGameRow :: Int -> PixeloGame -> PixeloGame
stepSolvePixeloGameRow rowId game =   
  PixeloGame
    newBoard 
    (pixeloGameGetRowSpecs game)
    (pixeloGameGetColSpecs game)
  where
    rowSpec = pixeloGameGetRowSpecs game !! rowId
    board = pixeloGameGetBoard game
    rowList = elems $ getRow rowId board
    newRowList = mergeSolutions $ generateSolutions rowSpec rowList
    newRow = listArray (0, length rowList - 1)  newRowList
    newBoard = setRow rowId newRow board

stepSolvePixeloGameCol :: Int -> PixeloGame -> PixeloGame
stepSolvePixeloGameCol colId game =   
  PixeloGame
    newBoard 
    (pixeloGameGetRowSpecs game)
    (pixeloGameGetColSpecs game)
  where
    colSpec = pixeloGameGetColSpecs game !! colId
    board = pixeloGameGetBoard game
    colList = elems $ getColumn colId board
    newColList = mergeSolutions $ generateSolutions colSpec colList
    newCol = listArray (0, length colList - 1)  newColList
    newBoard = setColumn colId newCol board

stepSolvePixeloGameRows :: PixeloGame -> PixeloGame
stepSolvePixeloGameRows game = 
  foldr1 (.) funs game
  where
    height = fst . snd . bounds $ pixeloGameGetBoard game
    funs = map (stepSolvePixeloGameRow) [0..height]

stepSolvePixeloGameCols :: PixeloGame -> PixeloGame
stepSolvePixeloGameCols game =
  foldr1 (.) funs game
  where
    width = snd . snd . bounds $ pixeloGameGetBoard game
    funs = map (stepSolvePixeloGameCol) [0..width]

stepSolvePixeloGame :: PixeloGame -> PixeloGame
stepSolvePixeloGame = stepSolvePixeloGameCols . stepSolvePixeloGameRows

solvePixeloGame :: PixeloGame -> PixeloGame
solvePixeloGame pixeloGame =   
  let 
    iteratedSols = iterate stepSolvePixeloGame pixeloGame
    pairedSkewedSols = zip iteratedSols (tail iteratedSols)
    terminatingSols = takeWhile (\(sol0, sol1) -> sol0 /= sol1) pairedSkewedSols
  in
    snd . last $ terminatingSols

-- Mouse clicks

setMousePosition :: Int -> Int -> IO ()
setMousePosition x y = do
  dpy <- X.openDisplay ""
  let dflt = X.defaultScreen dpy
  rootw <- X.rootWindow dpy dflt
  fakeMotion dpy dflt x y
  X.closeDisplay dpy

clickMouseButton :: IO ()
clickMouseButton = do
  dpy <- X.openDisplay ""
  fakeButtonPress dpy X.button1
  X.sync dpy False
  X.closeDisplay dpy

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
      setMousePosition mouseX mouseY
      clickMouseButton
      threadDelay 10000
        
    processGameTile _ = return ()


-- Pixbuf and Gtk functions

pixbufToColorMap :: Pixbuf -> IO ColorMap
pixbufToColorMap pixbuf = do
  (pixbufData :: PixbufData Int Word8) <- pixbufGetPixels pixbuf
  (frozenPixbufData :: Array Int Word8) <- freeze pixbufData
  width <- pixbufGetWidth pixbuf
  height <- pixbufGetHeight pixbuf
  rowstride <- pixbufGetRowstride pixbuf
  nChannels <- pixbufGetNChannels pixbuf
  let colorMapSize = (height - 1, width - 1)
      offsetMap = array
        ((0,0), colorMapSize)
        [((y, x), y * rowstride + x * nChannels) |
            (y, x) <- range ((0, 0), colorMapSize)]
  return $ amap (\offset -> (frozenPixbufData ! offset, frozenPixbufData ! (offset + 1), frozenPixbufData ! (offset + 2))) offsetMap

loadPixbuf :: IO (Maybe Pixbuf)
loadPixbuf = do
  Just screen <- screenGetDefault
  window <- screenGetRootWindow screen
  size <- drawableGetSize window
  origin <- drawWindowGetOrigin window
  pixbufGetFromDrawable window ((uncurry . uncurry Rectangle) origin size)

takePxBuf = do
  threadDelay 1000000
  Just pxbuf <- loadPixbuf
  return pxbuf

solvePxBuf pxBuf = do
  colorMap <- pixbufToColorMap pxBuf
  let Just pixeloBoard = findPixeloBoard colorMap
  pixeloGame <- colorMapToPixeloGame colorMap pixeloBoard
  let solvedGame = solvePixeloGame pixeloGame
  putStrLn . prettyPrintBoard . pixeloGameGetBoard $ solvedGame
  return (pixeloBoard, solvedGame)


main = WX.start gui

run = takePxBuf >>= solvePxBuf >>= (uncurry playGame)

gui = do
  f <- WX.frame [WX.text WX.:= "Pixelo Solver"]
  ok <- WX.button f [WX.text WX.:= "Solve it",
    WX.on WX.command WX.:= run]
  return ()
