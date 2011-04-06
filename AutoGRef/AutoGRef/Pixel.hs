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

intensity :: Pixel -> Double
intensity (RGBPixel8 r g b) = (r' + g' + b') / 3 / 256
  where r' = fromIntegral r
        g' = fromIntegral g
        b' = fromIntegral b

getRGBPixel8 :: Get Pixel
getRGBPixel8 = RGBPixel8 <$> getWord8 <*> getWord8 <*> getWord8

