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

  , imageLength = error "no default imageLength"
  , imageWidth = error "no default imageWidth"
  , photoMetricInterpretation =
          error "no default photoMetricInterpretation"
  , xResolution = error "no default xResolution"
  , yResolution = error "no default yResolution"

  , rowsPerStrip = 2^32 - 1
  , stripByteCounts = error "no default stripByteCounts"
  , stripOffsets = error "no default stripOffsets"
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

