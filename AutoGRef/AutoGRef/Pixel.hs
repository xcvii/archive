module AutoGRef.Pixel
  --(
  --)
  where

import Data.Word

data Pixel =
    RGBPixel Int Int Int
  | RGBPixel8 Word8 Word8 Word8
  deriving (Show)

intensity :: Pixel -> Int
intensity (RGBPixel r g b) = (r + g + b) `div` 3
intensity (RGBPixel8 r g b) = fromIntegral $ (r + g + b) `div` 3

