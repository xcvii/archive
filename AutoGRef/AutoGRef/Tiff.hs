module AutoGRef.Tiff
  --( Tiff (..)
  --, ByteOrder (..)
  --)
  where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import Control.Applicative ((<$>), (<*>))
import Control.Monad (replicateM)
import Data.Function (on)
import Data.List (sort)
import Data.Maybe (fromMaybe)
import GHC.Real ((%))

import AutoGRef.TiffInfo

import Debug.Trace

data Tiff = Tiff {
    tiffByteOrder :: ByteOrder
  , tiffInfo :: TiffInfo
  , tiffStrips :: [(Integer, BSL.ByteString)]
}
  deriving (Show)

instance Binary Tiff where
  get = do
    header <- lookAhead $ getTiffHeader

    let byteOrder = if tiffHeaderByteOrder header == tiffLE then LE else BE
    let fdiOffset = fromIntegral $ tiffHeaderFdiOffset header
    fields <- lookAhead $ getTiffFields byteOrder fdiOffset

    let info = foldr addField defaultTiffInfo fields
    let stripSorted = sort $ flip zip [1..] $ zip (stripOffsets info)
                                                  (stripByteCounts info)

    let stripSortedOffsets = map (fst . fst) stripSorted
    let stripSortedByteCounts = map (snd . fst) stripSorted
    let stripSortedOrdinals = map snd stripSorted

    strips <- mapM (uncurry $ getStrip byteOrder) $
                    zip stripSortedOffsets stripSortedByteCounts

    return Tiff { tiffByteOrder = byteOrder
                , tiffInfo = info
                , tiffStrips = zip stripSortedOrdinals strips }

  put tiff = do
    let byteOrder = tiffByteOrder tiff
    let info = tiffInfo tiff
    let dataSize = sum $ stripByteCounts info

    -- TIFF header
    putWord16le $ if byteOrder == LE then tiffLE else tiffBE
    putWord16 byteOrder tiffMagic
    putWord32 byteOrder . fromIntegral $ 8 + dataSize

    -- data strips
    mapM_ putLazyByteString . snd . unzip $ tiffStrips tiff

    -- FDI entry count
    putWord16 byteOrder $ fromIntegral 13

    -- BitsPerSample
    putWord16 byteOrder (tagId "BitsPerSample")
    putWord16 byteOrder (typeId "Short")
    putWord32 byteOrder . fromIntegral . length $ bitsPerSample info
    putWord32 byteOrder 0

    where tagId = fromMaybe 0 . flip lookup tagIds
          typeId = fromMaybe 0 . flip lookup typeIds

data TiffField = TiffField {
    fieldTag :: Word16
  , fieldValues :: [FieldValue]
}
  deriving (Show)

data FieldValue =
    Byte     { fromByte :: Word8 }
  | Ascii    { fromAscii :: Word8 }
  | Short    { fromShort :: Word16 }
  | Long     { fromLong :: Word32 }
  | Rational { numerator :: Word32
             , denominator :: Word32 }
  | Unknown
  deriving (Show)

typeIds :: [(String, Word16)]
typeIds =
  [ ("Byte",     1)
  , ("Ascii",    2)
  , ("Short",    3)
  , ("Long",     4)
  , ("Rational", 5)
  ]

data ByteOrder = LE | BE
  deriving (Eq, Show)

tiffLE :: Word16
tiffLE = 0x4949

tiffBE :: Word16
tiffBE = 0x4d4d

tiffMagic :: Word16
tiffMagic = 42

getWord16 :: ByteOrder -> Get Word16
getWord16 LE = getWord16le
getWord16 BE = getWord16be

getWord32 :: ByteOrder -> Get Word32
getWord32 LE = getWord32le
getWord32 BE = getWord32be

putWord16 :: ByteOrder -> Word16 -> Put
putWord16 LE = putWord16le
putWord16 BE = putWord16be

putWord32 :: ByteOrder -> Word32 -> Put
putWord32 LE = putWord32le
putWord32 BE = putWord32be

data TiffHeader = TiffHeader {
    tiffHeaderByteOrder :: Word16
  , tiffHeaderMagic :: Word16
  , tiffHeaderFdiOffset :: Word32
}
  deriving (Show)

getTiffHeader :: Get TiffHeader
getTiffHeader = do
  byteOrder <- getWord16le
  let order = if byteOrder == tiffLE then LE else BE

  magic <- getWord16 order
  offset <- getWord32 order

  return $ TiffHeader byteOrder magic offset

data TiffFdiEntry = TiffFdiEntry {
    tiffFdiEntryTag :: Word16
  , tiffFdiEntryType :: Word16
  , tiffFdiEntryValueCount :: Word32
  , tiffFdiEntryValueOffset :: Word32
}
  deriving (Show)

getTiffFdiEntry :: ByteOrder -> Get TiffFdiEntry
getTiffFdiEntry order =
  TiffFdiEntry <$> getWord16 order <*> getWord16 order
               <*> getWord32 order <*> getWord32 order

getTiffFields :: ByteOrder -> Int -> Get [TiffField]
getTiffFields byteOrder fdiOffset = do
  pos <- bytesRead >>= return . fromIntegral
  skip $ fdiOffset - pos

  entryCount <- getWord16 byteOrder
  replicateM (fromIntegral entryCount) $ do
    fdiEntry <- lookAhead $ getTiffFdiEntry byteOrder

    let valueType = tiffFdiEntryType fdiEntry
    let count = tiffFdiEntryValueCount fdiEntry

    pos <- bytesRead >>= return . fromIntegral
    let valueOffset =
          if (valueType == typeId "Byte" && count <= 4
             || valueType == typeId "Ascii" && count <= 4
             || valueType == typeId "Short" && count <= 2
             || valueType == typeId "Long" && count == 1)
            then pos + 8
            else tiffFdiEntryValueOffset fdiEntry

    values <- lookAhead $ do
      pos <- bytesRead >>= return . fromIntegral
      skip . fromIntegral $ valueOffset - pos

      replicateM (fromIntegral count) $
        case valueType of
          t | t == typeId "Byte" -> getWord8 >>= return . Byte
            | t == typeId "Ascii" -> getWord8 >>= return . Ascii
            | t == typeId "Short" -> getWord16 byteOrder >>= return . Short
            | t == typeId "Long"  -> getWord32 byteOrder >>= return . Long
            | t == typeId "Rational" -> Rational <$> getWord32 byteOrder
                                                 <*> getWord32 byteOrder
            | otherwise -> return Unknown

    getTiffFdiEntry byteOrder

    return TiffField { fieldTag = tiffFdiEntryTag fdiEntry
                     , fieldValues = values }

  where typeId = fromMaybe 0 . flip lookup typeIds

addField :: TiffField -> TiffInfo -> TiffInfo
addField field info

  | tag == tagId "BitsPerSample" = info {
      bitsPerSample = map (fromIntegral . fromShort) values }

  | tag == tagId "ColorMap" = info

  | tag == tagId "Compression" = info {
      compression = case fromShort $ head values of
                             t | t == 1 -> NoCompression
                               | t == 2 -> ModifiedHuffman
                               | t == 32773 -> PackBits    }

  | tag == tagId "ImageLength" = info {
      imageLength = case head values of Short s -> fromIntegral s
                                        Long l -> fromIntegral l  }

  | tag == tagId "ImageWidth" = info {
      imageWidth = case head values of Short s -> fromIntegral s
                                       Long l -> fromIntegral l  }

  | tag == tagId "PhotometricInterpretation" = info {
      photoMetricInterpretation = case fromShort $ head values of
                                   t | t == 0 -> WhiteIsZero
                                     | t == 1 -> BlackIsZero
                                     | t == 2 -> RGB
                                     | t == 3 -> Palette
                                     | t == 4 -> TransparencyMask }

  | tag == tagId "ResolutionUnit" = info {
      resolutionUnit = case fromShort $ head values of
                                t | t == 1 -> NoUnit
                                  | t == 2 -> Inch
                                  | t == 3 -> Centimeter }

  | tag == tagId "RowsPerStrip" = info {
      rowsPerStrip = case head values of Short s -> fromIntegral s
                                         Long l -> fromIntegral l  }

  | tag == tagId "SamplesPerPixel" = info {
      samplesPerPixel = fromIntegral . fromShort $ head values }

  | tag == tagId "StripByteCounts" = info {
      stripByteCounts = case head values of
                          Short _ -> map (fromIntegral . fromShort) values
                          Long _ -> map (fromIntegral . fromLong) values   }

  | tag == tagId "StripOffsets" = info {
      stripOffsets = case head values of
                          Short _ -> map (fromIntegral . fromShort) values
                          Long _ -> map (fromIntegral . fromLong) values   }

  | tag == tagId "XResolution" = info {
      xResolution = case head values of
                            Rational {numerator = n, denominator = d}
                              -> fromIntegral n % fromIntegral d      }

  | tag == tagId "YResolution" = info {
      yResolution = case head values of
                            Rational {numerator = n, denominator = d}
                              -> fromIntegral n % fromIntegral d      }

  | otherwise = info

  where tag = fieldTag field
        tagId = fromMaybe 0 . flip lookup tagIds
        values = fieldValues field

getStrip :: ByteOrder -> Integer -> Integer -> Get BSL.ByteString
getStrip order from to = do
  pos <- bytesRead >>= return . fromIntegral
  skip $ (fromInteger from) - pos

  getLazyByteString $ fromInteger to


tagIds :: [(String, Word16)]
tagIds =
  [ ("BitsPerSample",             258)
  , ("ColorMap",                  320)
  , ("Compression",               259)
  , ("ImageLength",               257)
  , ("ImageWidth",                256)
  , ("PhotometricInterpretation", 262)
  , ("ResolutionUnit",            296)
  , ("RowsPerStrip",              278)
  , ("SamplesPerPixel",           277)
  , ("StripByteCounts",           279)
  , ("StripOffsets",              273)
  , ("XResolution",               282)
  , ("YResolution",               283)
  ]

