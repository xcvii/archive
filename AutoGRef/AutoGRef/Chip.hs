{-# LANGUAGE NewQualifiedOperators #-}

module AutoGRef.Chip
  --(
  --)
  where

import Data.Array hiding ((!))
import qualified Data.Array as A

import Data.Vector

import AutoGRef.Tiff
import AutoGRef.TiffInfo
import AutoGRef.Pixel

data Chip = Chip {
    chipWidth :: Int
  , chipData :: Array Int Pixel
}
  deriving (Show)

(!) :: Chip -> (Int, Int) -> Pixel
(!) (Chip width dat) (row, col)
  | width <= col                       = error errMsg
  | not . flip inRange ix $ bounds dat = error errMsg
  | otherwise                          = dat `A.(!)` ix
  where ix = row * width + col
        errMsg = "index " ++ show (row, col) ++ " out of range"

