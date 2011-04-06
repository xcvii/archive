{-# LANGUAGE NewQualifiedOperators #-}

module AutoGRef.Chip
  --(
  --)
  where

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.ByteString.Lazy as BSL
import Data.STRef
import Control.Monad
import Data.Word
import Data.Binary.Get

import AutoGRef.Tiff
import AutoGRef.TiffInfo
import AutoGRef.Pixel

import Debug.Trace

data Chip = Chip {
    chipWidth :: Int
  , chipHeight :: Int
  , chipData :: V.Vector Pixel
}
  deriving (Show)

(!) :: Chip -> (Int, Int) -> Pixel
(!) (Chip width height vec) (col, row)
  | width <= col || col < 0  = error errMsg
  | height <= row || row < 0 = error errMsg
  | otherwise                = vec `V.(!)` ix
  where ix = row * width + col
        errMsg = "index " ++ show (col, row)
          ++ " out of range " ++ show (width - 1, height - 1)

getChip :: (Int, Int) -> Tiff -> (Int, Int) -> Chip
getChip (width, height) tiff (col, row) =
  let vec = V.create $ do
      let stripHeight = fromIntegral . rowsPerStrip $ tiffInfo tiff
      let rowWidth = fromIntegral . imageWidth $ tiffInfo tiff
      let pixelByteCount = flip div 8 . fromIntegral
              . sum . bitsPerSample $ tiffInfo tiff

      result <- MV.new $ width * height
      strips <- newSTRef $ tiffStrips tiff

      replicateM_ (row `div` stripHeight) $ modifySTRef strips tail
      replicateM_ (row `mod` stripHeight) . modifySTRef strips $ \(r:rs) ->
        (BSL.drop rowWidth r) : rs

      forM_ [0..height - 1] $ \i -> do
        strip <- liftM head $ readSTRef strips
        when (BSL.null strip) $ modifySTRef strips tail

        h <- liftM (BSL.take rowWidth) . liftM head $ readSTRef strips
        line <- newSTRef $ BSL.drop (fromIntegral col) h
        modifySTRef strips $ \(r:rs) -> (BSL.drop rowWidth r) : rs

        forM_ [0..width - 1] $ \j -> do
          l <- readSTRef line
          MV.write result (i * width + j) $ runGet getRGBPixel8 l
          modifySTRef line $ BSL.drop $ pixelByteCount

      return result
    
  in Chip width height vec

