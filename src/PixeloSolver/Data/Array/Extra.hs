module PixeloSolver.Data.Array.Extra (
  getColumn
  , setColumn
  , getRow
  , setRow
) where
import Data.Array.IArray

getRow :: (IArray a e, Ix i) => i -> a (i, i) e -> a i e
getRow row arr = ixmap (firstRowIdx, lastRowIdx) (\e -> (row, e)) arr
  where
    firstRowIdx = snd . fst . bounds $ arr
    lastRowIdx = snd . snd . bounds $ arr

setRow :: (IArray a e, Ix i) => i -> a i e -> a (i, i) e -> a (i, i) e
setRow rowIdx row inputMap =
  inputMap // (map (\(x, value) -> ((rowIdx, x), value)) (assocs row))

getColumn :: (IArray a e, Ix i) => i -> a (i, i) e -> a i e
getColumn column arr = ixmap
  (firstColumnIdx, lastColumnIdx)
  (\i -> (i, column))
  arr
  where
    firstColumnIdx = fst . fst . bounds $ arr
    lastColumnIdx = fst . snd . bounds $ arr

setColumn :: (IArray a e, Ix i) => i -> a i e -> a (i, i) e -> a (i, i) e
setColumn colIdx column inputMap =
  inputMap // (map (\(y, value) -> ((y, colIdx), value)) (assocs column))
