module AutoGRef.Pixel
  --(
  --)
  where

import Data.Binary.Get
import Data.Word

import Control.Applicative ((<$>), (<*>))

data Pixel =
    RGBPixel8 Word8 Word8 Word8
  -- more might be added
  deriving (Show)

intensity :: Pixel -> Int
intensity (RGBPixel8 r g b) = fromIntegral $ (r + g + b) `div` 3

getRGBPixel8 :: Get Pixel
getRGBPixel8 = RGBPixel8 <$> getWord8 <*> getWord8 <*> getWord8

