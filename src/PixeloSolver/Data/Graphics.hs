{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
module PixeloSolver.Data.Graphics(
  RGB
  , BW
  , BlackCheckable(..)
  , WhiteCheckable(..)
  , Row(..)
  , Map(..)
  , UnboxedColorMap(..)
  , BoxedMap
  , prettyPrintBWMap
  , mapToBWMap
  ) where

import Data.Array.IArray
import Data.Array.Unboxed(UArray)
import Data.Word

type RGB = (Word8, Word8, Word8)

white :: RGB
white = (255, 255, 255)

blackThreshold :: Word8
blackThreshold = 20

class Row r e where
  rowGet :: Int -> r e -> e

  rowGetSize :: r e -> Int

  rowAssocs :: r e -> [(Int, e)]

  rowElems :: r e -> [e]
  rowElems = map snd . rowAssocs

class (Row r e) => Map m r e | m -> r where
  mapGet :: (Int, Int) -> m e -> e

  mapGetWidth :: m e -> Int

  mapGetHeight :: m e -> Int

  mapGetSize :: m e -> (Int, Int)
  mapGetSize m = (mapGetHeight m, mapGetWidth m)

  submap :: ((Int, Int), (Int, Int)) -> m e -> m e

  mapGetRow :: Int -> m e -> r e

  mapGetColumn :: Int -> m e -> r e

data BW = Black | White deriving (Eq, Show)

class BlackCheckable e where
  isBlack :: e -> Bool

instance BlackCheckable BW where
  isBlack Black = True
  isBlack _ = False

instance BlackCheckable RGB where
  isBlack (r, g, b) = r == g && g == b && r < blackThreshold

class WhiteCheckable e where
  isWhite :: e -> Bool

instance WhiteCheckable BW where
  isWhite White = True
  isWhite _ = False

instance WhiteCheckable RGB where
  isWhite = (== white)

type UnboxedColorMapIx = (Int, Int, Int)
newtype UnboxedColorRow e = UCR { getUCR :: (UArray (Int, Int) Word8) }
newtype UnboxedColorMap e = UCM { getUCM :: (UArray UnboxedColorMapIx Word8) }

instance Row UnboxedColorRow RGB where
  rowGet x (UCR uCR) = (uCR ! (x, 0), uCR ! (x, 1), uCR ! (x, 2))

  rowGetSize (UCR uCR) = fst . snd . bounds $ uCR

  rowAssocs uCR = map (\i -> (i, rowGet i uCR)) [0..(rowGetSize uCR)]

instance Map UnboxedColorMap UnboxedColorRow RGB where
  mapGet (y, x) (UCM uCM) = (uCM ! (y, x, 0), uCM ! (y, x, 1), uCM ! (y, x, 2))

  mapGetWidth = (\(_, s, _) -> s) . snd . bounds . getUCM

  mapGetHeight = (\(s, _, _) -> s) . snd . bounds . getUCM

  submap ((begY, begX), (endY, endX)) =
    UCM
    . ixmap ((0, 0, 0), (height, width, 2))
      (\(y, x, c) -> (y + begY, x + begX, c)) 
    . getUCM
    where
      (height, width) = (endY - begY, endX - begX)

  mapGetRow rowId uCM = 
    UCR
    . ixmap ((0, 0), (width, 2)) 
      (\(x, c) -> (rowId, x, c)) 
    . getUCM
    $ uCM
    where
      width = mapGetWidth uCM

  mapGetColumn columnId uCM =
    UCR
    . ixmap ((0, 0), (height, 2))
      (\(y, c) -> (y, columnId, c))
    . getUCM
    $ uCM
    where
      height = mapGetHeight uCM

type BoxedRow = Array Int
type BoxedMap = Array (Int, Int)
type BWRow = BoxedRow BW
type BWMap = BoxedMap BW
type BoxedColorMap = BoxedMap RGB

instance Row BoxedRow e where
  rowGet x = (! x)

  rowGetSize = snd . bounds

  rowAssocs = assocs

instance Map BoxedMap BoxedRow e where
  mapGet (y, x) = (! (y, x))

  mapGetWidth = snd . snd . bounds

  mapGetHeight = fst . snd . bounds

  submap ((begY, begX), (endY, endX)) =
    ixmap ((0, 0), (height, width)) (\(y, x) -> (y + begY, x + begX))
    where
      (height, width) = (endY - begY, endX - begX)

  mapGetRow rowId bM = ixmap (0, width) (\x -> (rowId, x)) bM
    where
      width = mapGetWidth bM

  mapGetColumn columnId bM = ixmap (0, height) (\y -> (y, columnId)) bM
    where
      height = mapGetHeight bM

mapToBWMap :: (BlackCheckable e, Map m r e) => m e -> BWMap
mapToBWMap m = listArray boundaries 
  [filterBlack . mapGet i $ m | i <- range boundaries]
  where 
    (height, width) = mapGetSize m
    boundaries = ((0, 0), (height, width))
    filterBlack = \c -> if isBlack c then Black else White

prettyPrintBW :: BW -> Char
prettyPrintBW Black = '#'
prettyPrintBW White = '_'

prettyPrintBWMapRow :: BWRow -> String
prettyPrintBWMapRow bwMapRow = 
  (map prettyPrintBW (elems bwMapRow)) ++ ['\n']

prettyPrintBWMap :: BWMap -> String
prettyPrintBWMap bwMap = 
  concat $ map (\i -> prettyPrintBWMapRow $ mapGetRow i bwMap) [0..height]
  where
    height = mapGetHeight bwMap
