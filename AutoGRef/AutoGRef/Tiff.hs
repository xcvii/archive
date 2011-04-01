module AutoGRef.Tiff
  --( Tiff (..)
  --)
  where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Word

import AutoGRef.TiffInfo

data Tiff = Tiff {
    tiffInfo :: TiffInfo
}
  deriving (Show)

data TiffField = TiffField {
    fieldTag :: Word16
  , fieldValues :: [FieldValue]
}
  deriving (Show)

data FieldValue =
    Byte Word8
  | Ascii String
  | Short Word16
  | Long Word32
  | Rational Word32 Word32
  deriving (Show)

