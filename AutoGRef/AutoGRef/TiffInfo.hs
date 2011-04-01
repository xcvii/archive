module AutoGRef.TiffInfo
  ( TiffInfo (..)
  , defaultTiffInfo
  )
  where

data TiffInfo = TiffInfo {
    bitsPerSample :: [Int]             -- 0x102
  , colorMap :: Maybe ColorMap         -- 0x140
  , compression :: Compression         -- 0x103
  , resolutionUnit :: ResolutionUnit   -- 0x128
  , samplesPerPixel :: Int             -- 0x115

  , imageLength :: Int                 -- 0x101
  , imageWidth :: Int                  -- 0x100
  , photoMetricInterpretation :: PMI   -- 0x106
  , xResolution :: Rational            -- 0x11a
  , yResolution :: Rational            -- 0x11b
}
  deriving (Show)

defaultTiffInfo = TiffInfo {
    bitsPerSample = [1]
  , colorMap = Nothing
  , compression = NoCompression
  , resolutionUnit = Inch
  , samplesPerPixel = 1

  , imageLength = error ""
  , imageWidth = error ""
  , photoMetricInterpretation = error ""
  , xResolution = error ""
  , yResolution = error ""
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

