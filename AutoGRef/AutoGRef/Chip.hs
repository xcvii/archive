{-# LANGUAGE NewQualifiedOperators #-}

module AutoGRef.Chip
  --(
  --)
  where

import qualified Data.Vector as V

import AutoGRef.Tiff
import AutoGRef.TiffInfo
import AutoGRef.Pixel

data Chip = Chip {
    chipWidth :: Int
  , chipHeight :: Int
  , chipData :: V.Vector Pixel
}
  deriving (Show)

(!) :: Chip -> (Int, Int) -> Pixel
(!) (Chip width height vec) (row, col)
  | width <= col || col < 0  = error errMsg
  | height <= row || row < 0 = error errMsg
  | otherwise                = vec `V.(!)` ix
  where ix = row * width + col
        errMsg = "index " ++ show (row, col)
          ++ " out of range " ++ show (width - 1, height - 1)

