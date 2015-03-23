{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module System.Utils where

import Data.Array.IArray
import Data.Array.IO
import Data.Array.Unboxed(UArray)
import Data.Graphics
import Data.Word
import Foreign.Storable

import qualified Graphics.X11.Xlib as X
import Graphics.X11.XTest

import qualified Graphics.UI.WXCore as WXC
import qualified Graphics.UI.WXCore.WxcTypes as WXC

-- Screenshot helpers

getScreenShot :: IO (UnboxedColorMap e)
getScreenShot = do
  screenDC :: (WXC.ScreenDC ()) <- WXC.screenDCCreate
  dcSize <- WXC.dcGetSize screenDC
  bitmap :: WXC.Bitmap () <- WXC.bitmapCreateEmpty dcSize 24
  memoryDC :: WXC.MemoryDC () <- WXC.memoryDCCreateWithBitmap bitmap
  let bitmapRect = WXC.rect (WXC.Point 0 0) dcSize
  _ <- WXC.dcBlit memoryDC bitmapRect screenDC (WXC.Point 0 0)
    WXC.wxCOPY
    False
  WXC.screenDCDelete screenDC
  image <- WXC.imageCreateFromBitmap bitmap
  WXC.memoryDCDelete memoryDC
  WXC.bitmapDelete bitmap
  ptr :: WXC.Ptr ()  <- (WXC.imageGetData image)
  let ptrWord = WXC.ptrCast ptr :: WXC.Ptr Word8
  colorMap <- ptrToColorMap 1920 1080 ptrWord
  WXC.imageDestroy image
  return $! colorMap

ptrToColorMap :: Int -> Int -> WXC.Ptr Word8 -> IO (UnboxedColorMap e)
ptrToColorMap width height p = do
  ioArr :: (IOUArray (Int, Int, Int) Word8) <- newListArray ((0, 0, 0), (height - 1, width - 1, 2)) (repeat 0)
  mapM_ (\(y, x) -> x `seq` y `seq` do
    red <- peekElemOff p (y * width * 3 + x * 3)
    green <- peekElemOff p (y * width * 3 + x * 3 + 1)
    blue <- peekElemOff p (y * width * 3 + x * 3 + 2)
    writeArray ioArr (y, x, 0) red
    writeArray ioArr (y, x, 1) green
    writeArray ioArr (y, x, 2) blue)
    (range ((0, 0), (height - 1, width - 1)))
  arr <- freeze ioArr :: IO (UArray (Int, Int, Int) Word8)
  return . UCM $! arr

-- Mouse manipulation

setMousePosition :: Int -> Int -> IO ()
setMousePosition x y = do
  dpy <- X.openDisplay ""
  let dflt = X.defaultScreen dpy
  _ <- X.rootWindow dpy dflt
  fakeMotion dpy dflt x y
  X.closeDisplay dpy

clickMouseButton :: IO ()
clickMouseButton = do
  dpy <- X.openDisplay ""
  fakeButtonPress dpy X.button1
  X.sync dpy False
  X.closeDisplay dpy

clickMouseButtonAt :: Int -> Int -> IO ()
clickMouseButtonAt x y  = do
  setMousePosition x y
  clickMouseButton
