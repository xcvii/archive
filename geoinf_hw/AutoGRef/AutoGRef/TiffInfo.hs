module AutoGRef.TiffInfo
  ( TiffInfo (..)
  , defaultTiffInfo
  , Compression (..)
  , ResolutionUnit (..)
  , PMI (..)
  )
  where

import Data.Ratio
import Data.Word

data TiffInfo = TiffInfo {
    bitsPerSample :: [Word16]
  , colorMap :: Maybe ColorMap
  , compression :: Compression
  , resolutionUnit :: ResolutionUnit
  , samplesPerPixel :: Word16

  , imageLength :: Word32
  , imageWidth :: Word32
  , photoMetricInterpretation :: PMI
  , xResolution :: Ratio Word32
  , yResolution :: Ratio Word32

  , rowsPerStrip :: Word32
  , stripByteCounts :: [Word32]
  , stripOffsets :: [Word32]
}
  deriving (Show)

defaultTiffInfo = TiffInfo {
    bitsPerSample = [1]
  , colorMap = Nothing
  , compression = NoCompression
  , resolutionUnit = Inch
  , samplesPerPixel = 1

  , imageLength = error "imageLength not set"
  , imageWidth = error "imageWidth not set"
  , photoMetricInterpretation =
          error "photoMetricInterpretation not set"
  , xResolution = error "xResolution not set"
  , yResolution = error "yResolution not set"

  , rowsPerStrip = 2^32 - 1
  , stripByteCounts = error "stripByteCounts not set"
  , stripOffsets = error "stripOffsets not set"
}

data PMI =
    WhiteIsZero
  | BlackIsZero
  | RGB
  | Palette
  | TransparencyMask
  deriving (Eq, Show)

type ColorMap = () 

data Compression =
    NoCompression
  | ModifiedHuffman
  | PackBits
  deriving (Eq, Show)

data ResolutionUnit =
    NoUnit
  | Inch
  | Centimeter
  deriving (Eq, Show)

