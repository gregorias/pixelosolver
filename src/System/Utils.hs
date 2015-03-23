{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module System.Utils where

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Data.Array.IArray
import Data.Array.IO
import Data.Array.MArray
import Data.Array.Unboxed(UArray)
import Data.Graphics
import Data.IORef
import Data.Word
import Foreign.Storable

import Graphics.UI.Gtk

import qualified Graphics.X11.Xlib as X
import Graphics.X11.XTest

import qualified Graphics.UI.WXCore as WXC
import qualified Graphics.UI.WXCore.WxcTypes as WXC

-- Screenshot helpers

loadPixbuf :: IO (Maybe Pixbuf)
loadPixbuf = do
  Just screen <- screenGetDefault
  window <- screenGetRootWindow screen
  size <- drawableGetSize window
  origin <- drawWindowGetOrigin window
  pixbufGetFromDrawable window ((uncurry . uncurry Rectangle) origin size)

takePxBuf :: IO Pixbuf
takePxBuf = do
  Just pxbuf <- loadPixbuf
  return pxbuf

pixbufToColorMap :: Pixbuf -> IO BoxedColorMap
pixbufToColorMap pixbuf = do
  (pixbufData :: PixbufData Int Word8) <- pixbufGetPixels pixbuf
  (frozenPixbufData :: Array Int Word8) <- unsafeFreeze pixbufData
  width <- pixbufGetWidth pixbuf
  height <- pixbufGetHeight pixbuf
  rowstride <- pixbufGetRowstride pixbuf
  nChannels <- pixbufGetNChannels pixbuf
  let colorMapSize = (height - 1, width - 1)
      offsetMap = array
        ((0,0), colorMapSize)
        [((y, x), y * rowstride + x * nChannels) |
            (y, x) <- range ((0, 0), colorMapSize)]
  return $!! (offsetMap `deepseq` amap 
    (\offset -> (frozenPixbufData ! offset,
      frozenPixbufData ! (offset + 1),
      frozenPixbufData ! (offset + 2)))
    offsetMap)

pixbufToArray :: Pixbuf -> IO (Array Int Word8)
pixbufToArray pixbuf = do
  (pixbufData :: PixbufData Int Word8) <- pixbufGetPixels pixbuf
  (frozenPixbufData :: Array Int Word8) <- freeze pixbufData
  return $!! frozenPixbufData

getScreenShot :: IO BoxedColorMap
getScreenShot = do
  screenDC :: (WXC.ScreenDC ()) <- WXC.screenDCCreate
  dcSize <- WXC.dcGetSize screenDC
  bitmap :: WXC.Bitmap () <- WXC.bitmapCreateEmpty dcSize 24
  memoryDC :: WXC.MemoryDC () <- WXC.memoryDCCreateWithBitmap bitmap
  let bitmapRect = WXC.rect (WXC.Point 0 0) dcSize
  isSuccess <- WXC.dcBlit memoryDC bitmapRect screenDC (WXC.Point 0 0)
    WXC.wxCOPY
    False
  image <- WXC.imageCreateFromBitmap bitmap
  putStrLn "Converting to colorMap"
  colorMap <- imageToColorMap image
  WXC.imageDestroy image
  WXC.memoryDCDelete memoryDC
  WXC.bitmapDelete bitmap
  WXC.screenDCDelete screenDC
  return colorMap

instance NFData WXC.Color
instance NFData (WXC.Point2 a)

-- getScreenShot2 :: (IArray a WXC.Color) => IO (a WXC.Point WXC.Color)
getScreenShot2 :: IO (Array WXC.Point WXC.Color)
getScreenShot2 = do
  screenDC :: (WXC.ScreenDC ()) <- WXC.screenDCCreate
  dcSize <- WXC.dcGetSize screenDC
  bitmap :: WXC.Bitmap () <- WXC.bitmapCreateEmpty dcSize 24
  memoryDC :: WXC.MemoryDC () <- WXC.memoryDCCreateWithBitmap bitmap
  let bitmapRect = WXC.rect (WXC.Point 0 0) dcSize
  isSuccess <- WXC.dcBlit memoryDC bitmapRect screenDC (WXC.Point 0 0)
    WXC.wxCOPY
    False
  image <- WXC.imageCreateFromBitmap bitmap
  putStrLn "Converting to colorMap"
  colorMap <- WXC.imageGetPixelArray image
  WXC.imageDestroy image
  WXC.memoryDCDelete memoryDC
  WXC.bitmapDelete bitmap
  WXC.screenDCDelete screenDC
  return $!! colorMap

getScreenShot3 :: IO (UnboxedColorMap e)
getScreenShot3 = do
  screenDC :: (WXC.ScreenDC ()) <- WXC.screenDCCreate
  dcSize <- WXC.dcGetSize screenDC
  bitmap :: WXC.Bitmap () <- WXC.bitmapCreateEmpty dcSize 24
  memoryDC :: WXC.MemoryDC () <- WXC.memoryDCCreateWithBitmap bitmap
  let bitmapRect = WXC.rect (WXC.Point 0 0) dcSize
  isSuccess <- WXC.dcBlit memoryDC bitmapRect screenDC (WXC.Point 0 0)
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

{- ptrToColorMap :: WXC.Ptr Word8 -> IO ColorMap
ptrToColorMap p = do
  elems <- mapM (\(x, y) -> x `seq` y `seq` do
    red <- peekElemOff p (y * width * 3 + x)
    green <- peekElemOff p (y * width * 3 + x + 1)
    blue <- peekElemOff p (y * width * 3 + x + 2)
    red `seq` green `seq` blue `seq` return $!! (red, green, blue)) 
    (range ((0, 0), (width, 1080)))
  return $!! listArray ((0, 0), (width, 1080)) elems
  where
    width = 1920 -}
    
{- ptrToColorMap :: WXC.Ptr Word8 -> IO ()
ptrToColorMap p = do
  ioArr :: (IOArray (Int, Int) RGB) <- newListArray ((0, 0), (height, width)) (repeat (0, 0, 0))
  mapM_ (\(y, x) -> x `seq` y `seq` do
    red <- peekElemOff p (y * width * 3 + x)
    green <- peekElemOff p (y * width * 3 + x + 1)
    blue <- peekElemOff p (y * width * 3 + x + 2)
    writeArray ioArr (y, x) (red, green, blue))
    (range ((0, 0), (1080, width)))
  return ()
  where
    (width, height) = (1920, 1080) -}

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

{- ptrToColorMap :: WXC.Ptr Word8 -> IO ()
ptrToColorMap p = do
  ioRefArr <- newIORef $ (listArray ((0, 0, 0), (height, width, 2)) (repeat 0) :: UArray.UArray (Int, Int, Int) Word8)
  mapM_ (\(y, x) -> x `seq` y `seq` do
    red <- peekElemOff p (y * width * 3 + x)
    green <- peekElemOff p (y * width * 3 + x + 1)
    blue <- peekElemOff p (y * width * 3 + x + 2)
    putStrLn $ show (y, x)
    modifyIORef' ioRefArr 
      (\a -> a // [ ((y, x, 0), red), ((y, x, 1), green), ((y, x, 2), blue)]))
    (range ((0, 0), (1080, width)))
  arr <- readIORef ioRefArr
  return ()
  where
    (width, height) = (1920, 1080) -}

imageColorMapToLazyArray :: (Array WXC.Point WXC.Color) -> BoxedColorMap
imageColorMapToLazyArray imageArray = 
  amap (\c -> toWord (WXC.colorRed c, WXC.colorGreen c, WXC.colorBlue c)) imageArray'
  where
    (width, height) = (,) <$> WXC.pointX <*> WXC.pointY $ snd . bounds $ imageArray
    imageArray' = ixmap ((0, 0), (height, width))
      (\(y, x) -> WXC.Point x y)
      imageArray

    toWord = \(r, g, b) -> (toEnum . fromEnum $ r
      , toEnum . fromEnum $ g
      , toEnum . fromEnum $ b)


imageToColorMap :: (WXC.Image ()) -> IO BoxedColorMap
imageToColorMap image =
  let
      (height, width) = (900, 1600)
      colorMapSize = (height - 1, width - 1)
      mapRange = range ((0,0), colorMapSize)
      offsetMap :: Array (Int, Int) (Int, Int) = array
        ((0,0), colorMapSize)
        [((y, x), (y, x)) | (y, x) <- range ((0, 0), colorMapSize)]
  in do
    mapElems <- mapM
      (\(y, x) -> do 
        red <- liftM c2w $ WXC.imageGetRed image (WXC.Point x y)
        green <- liftM c2w $ WXC.imageGetGreen image (WXC.Point x y)
        blue <- liftM c2w $ WXC.imageGetBlue image (WXC.Point x y)
        return (red, green, blue))
      mapRange 
    let colorMap = listArray ((0, 0), colorMapSize) mapElems
    return $!! colorMap

c2w :: Char -> Word8
c2w = toEnum . fromEnum
  

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
