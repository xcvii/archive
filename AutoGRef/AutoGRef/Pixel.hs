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

getRGBPixel8 :: Get Pixel
getRGBPixel8 = RGBPixel8 <$> getWord8 <*> getWord8 <*> getWord8

intensity :: Pixel -> Double
intensity (RGBPixel8 r g b) = (r' + g' + b') / 3 / 256
  where r' = fromIntegral r
        g' = fromIntegral g
        b' = fromIntegral b

hue :: Pixel -> Double
hue (RGBPixel8 r g b) = atan2 ((sqrt 3) * (g' - b')) (2 * r' - g' - b')
  where r' = fromIntegral r
        g' = fromIntegral g
        b' = fromIntegral b

