module AutoGRef.TiffInfo
  ( TiffInfo (..)
  , defaultTiffInfo
  , Compression (..)
  , ResolutionUnit (..)
  , PMI (..)
  )
  where

data TiffInfo = TiffInfo {
    bitsPerSample :: [Integer]
  , colorMap :: Maybe ColorMap
  , compression :: Compression
  , resolutionUnit :: ResolutionUnit
  , samplesPerPixel :: Int

  , imageLength :: Integer
  , imageWidth :: Integer
  , photoMetricInterpretation :: PMI
  , xResolution :: Rational
  , yResolution :: Rational

  , rowsPerStrip :: Integer
  , stripByteCounts :: [Integer]
  , stripOffsets :: [Integer]
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
  deriving (Show)

type ColorMap = () 

data Compression =
    NoCompression
  | ModifiedHuffman
  | PackBits
  deriving (Show)

data ResolutionUnit =
    NoUnit
  | Inch
  | Centimeter
  deriving (Show)

