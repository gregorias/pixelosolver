module PixeloSolver.Data.Array.Extra (
  getColumn
  , setColumn
  , getRow
  , setRow
) where
import Data.Array.IArray

getRow :: (IArray a e, Ix i) => i -> a (i, i) e -> a i e
getRow row map = ixmap (firstRowIdx, lastRowIdx) (\e -> (row, e)) map
  where
    firstRowIdx = snd . fst . bounds $ map
    lastRowIdx = snd . snd . bounds $ map

setRow :: (IArray a e, Ix i) => i -> a i e -> a (i, i) e -> a (i, i) e
setRow rowIdx row inputMap =
  inputMap // (map (\(x, value) -> ((rowIdx, x), value)) (assocs row))

getColumn :: (IArray a e, Ix i) => i -> a (i, i) e -> a i e
getColumn column map = ixmap
  (firstColumnIdx, lastColumnIdx)
  (\i -> (i, column))
  map
  where
    firstColumnIdx = fst . fst . bounds $ map
    lastColumnIdx = fst . snd . bounds $ map

setColumn :: (IArray a e, Ix i) => i -> a i e -> a (i, i) e -> a (i, i) e
setColumn colIdx column inputMap =
  inputMap // (map (\(y, value) -> ((y, colIdx), value)) (assocs column))
