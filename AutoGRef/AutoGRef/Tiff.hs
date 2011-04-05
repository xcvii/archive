module AutoGRef.Tiff
  --( Tiff (..)
  --, ByteOrder (..)
  --)
  where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.Binary.Builder as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import Control.Applicative ((<$>), (<*>))
import Control.Monad (liftM, replicateM)
import Data.List (sort)
import Data.Maybe (fromMaybe)
import qualified Data.Ratio as R
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
    let fdiOffset = tiffHeaderFdiOffset header
    fields <- lookAhead $ getTiffFields byteOrder fdiOffset

    let info = foldr addField defaultTiffInfo fields
    let stripSorted = sort . flip zip [1..] $ zip (stripOffsets info)
                                                  (stripByteCounts info)

    let stripSortedOffsets = map (fst . fst) stripSorted
    let stripSortedByteCounts = map (snd . fst) stripSorted
    let stripSortedOrdinals = map snd stripSorted

    strips <- mapM (uncurry $ getStrip byteOrder)
                $ zip stripSortedOffsets stripSortedByteCounts

    return Tiff { tiffByteOrder = byteOrder
                , tiffInfo = info
                , tiffStrips = zip stripSortedOrdinals strips }

  put tiff = do
    let byteOrder = tiffByteOrder tiff
    let info = tiffInfo tiff
    let dataOffset = 8
    let dataSize = fromIntegral . sum $ stripByteCounts info
    let fdiOffset = dataOffset + dataSize

    let info' = info {
      stripOffsets = scanl (+) dataOffset . init $ stripByteCounts info }

    -- 0x00: TIFF header
    putWord16le $ if byteOrder == LE then tiffLE else tiffBE
    putWord16 byteOrder tiffMagic
    putWord32 byteOrder fdiOffset

    -- 0x08: data strips
    mapM_ putLazyByteString . snd . unzip $ tiffStrips tiff

    -- 0x08 + dataSize: FDI
    let fdiData = fdiFromInfo byteOrder info' fdiOffset
    putLazyByteString fdiData


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

typeId :: String -> Word16
typeId = fromMaybe 0 . flip lookup typeIds

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

getTiffFields :: ByteOrder -> Word32 -> Get [TiffField]
getTiffFields byteOrder fdiOffset = do
  pos <- liftM fromIntegral bytesRead
  skip $ (fromIntegral fdiOffset) - pos

  entryCount <- getWord16 byteOrder
  replicateM (fromIntegral entryCount) $ do
    fdiEntry <- lookAhead $ getTiffFdiEntry byteOrder

    let valueType = tiffFdiEntryType fdiEntry
    let count = tiffFdiEntryValueCount fdiEntry

    pos <- liftM fromIntegral bytesRead
    let valueOffset =
          if (valueType == typeId "Byte" && count <= 4
             || valueType == typeId "Ascii" && count <= 4
             || valueType == typeId "Short" && count <= 2
             || valueType == typeId "Long" && count == 1)
            then pos + 8
            else tiffFdiEntryValueOffset fdiEntry

    values <- lookAhead $ do
      pos <- liftM fromIntegral bytesRead
      skip . fromIntegral $ valueOffset - pos

      replicateM (fromIntegral count)
        $ case valueType of
          t | t == typeId "Byte" -> liftM Byte getWord8
            | t == typeId "Ascii" -> liftM Ascii getWord8
            | t == typeId "Short" -> liftM Short $ getWord16 byteOrder
            | t == typeId "Long"  -> liftM Long $ getWord32 byteOrder
            | t == typeId "Rational" -> Rational <$> getWord32 byteOrder
                                                 <*> getWord32 byteOrder
            | otherwise -> return Unknown

    getTiffFdiEntry byteOrder

    return TiffField { fieldTag = tiffFdiEntryTag fdiEntry
                     , fieldValues = values }

addField :: TiffField -> TiffInfo -> TiffInfo
addField field info

  | tag == tagId "BitsPerSample" = info {
      bitsPerSample = map fromShort values }

  | tag == tagId "ColorMap" = info

  | tag == tagId "Compression" = info {
      compression = case fromShort $ head values of
                             t | t == 1 -> NoCompression
                               | t == 2 -> ModifiedHuffman
                               | t == 32773 -> PackBits    }

  | tag == tagId "ImageLength" = info {
      imageLength = case head values of Long l -> l
                                        Short s -> fromIntegral s }

  | tag == tagId "ImageWidth" = info {
      imageWidth = case head values of Long l -> l
                                       Short s -> fromIntegral s }

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
      rowsPerStrip = case head values of Long l -> l
                                         Short s -> fromIntegral s }

  | tag == tagId "SamplesPerPixel" = info {
      samplesPerPixel = fromShort $ head values }

  | tag == tagId "StripByteCounts" = info {
      stripByteCounts = case head values of
                          Long _ -> map fromLong values
                          Short _ -> map (fromIntegral . fromShort) values }

  | tag == tagId "StripOffsets" = info {
      stripOffsets = case head values of
                          Long _ -> map fromLong values
                          Short _ -> map (fromIntegral . fromShort) values }

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
        values = fieldValues field

getStrip :: ByteOrder -> Word32 -> Word32 -> Get BSL.ByteString
getStrip order from to = do
  pos <- liftM fromIntegral bytesRead
  skip $ (fromIntegral from) - pos

  getLazyByteString $ fromIntegral to


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

tagId :: String -> Word16
tagId = fromMaybe 0 . flip lookup tagIds

fdiFromInfo :: ByteOrder -> TiffInfo -> Word32 -> BSL.ByteString
fdiFromInfo byteOrder info offset =
  BSL.append
    (runPut . putWord16 byteOrder $ fromIntegral fieldCount)
    . BSL.concat . tail . snd . unzip
      $ scanl (flip ($) . fst) (offset + 2, undefined) fieldGenerators

  where
  fieldGenerators =
    [ bitsPerSampleChunk
    , compressionChunk
    , imageLengthChunk
    , imageWidthChunk
    , pmiChunk
    , resolutionUnitChunk
    , rowsPerStripChunk
    , samplesPerPixelChunk
    , stripByteCountsChunk
    , stripOffsetsChunk
    , xResolutionChunk
    , yResolutionChunk
    ]

  fieldCount = length fieldGenerators

  bitsPerSampleChunk offset =
    let output = runPut $ do
        putWord16 byteOrder $ tagId "BitsPerSample"
        putWord16 byteOrder $ typeId "Short"
        putWord32 byteOrder 0 -- ???
        putWord32 byteOrder 0 -- ???
    in (offset + fromIntegral (BSL.length output), output)

  compressionChunk offset = 
    let output = runPut $ do
        putWord16 byteOrder $ tagId "Compression"
        putWord16 byteOrder $ typeId "Short"
        putWord32 byteOrder 1
        putWord32 byteOrder $ case compression info of
                               t | t == NoCompression -> 1
                                 | t == ModifiedHuffman -> 2
                                 | t == PackBits -> 32773
    in (offset + 12, output)

  imageLengthChunk offset =
    let output = runPut $ do
        putWord16 byteOrder $ tagId "ImageLength"
        putWord16 byteOrder $ typeId "Long"
        putWord32 byteOrder 1
        putWord32 byteOrder $ imageLength info
    in (offset + 12, output)

  imageWidthChunk offset =
    let output = runPut $ do
        putWord16 byteOrder $ tagId "ImageWidth"
        putWord16 byteOrder $ typeId "Long"
        putWord32 byteOrder 1
        putWord32 byteOrder $ imageWidth info
    in (offset + 12, output)

  pmiChunk offset =
    let output = runPut $ do
        putWord16 byteOrder $ tagId "PhotometricInterpretation"
        putWord16 byteOrder $ typeId "Short"
        putWord32 byteOrder 1
        putWord32 byteOrder $ case photoMetricInterpretation info of
                               t | t == WhiteIsZero -> 0
                                 | t == BlackIsZero -> 1
                                 | t == RGB -> 2
                                 | t == Palette -> 3
                                 | t == TransparencyMask -> 4
    in (offset + 12, output)

  resolutionUnitChunk offset =
    let output = runPut $ do
        putWord16 byteOrder $ tagId "ResolutionUnit"
        putWord16 byteOrder $ typeId "Short"
        putWord32 byteOrder 1
        putWord32 byteOrder $ case resolutionUnit info of
                               t | t == NoUnit     -> 1
                                 | t == Inch       -> 2
                                 | t == Centimeter -> 3
    in (offset + 12, output)
                              
  rowsPerStripChunk offset =
    let output = runPut $ do
        putWord16 byteOrder $ tagId "RowsPerStrip"
        putWord16 byteOrder $ typeId "Long"
        putWord32 byteOrder 1
        putWord32 byteOrder $ rowsPerStrip info
    in (offset + 12, output)

  samplesPerPixelChunk offset =
    let output = runPut $ do
        putWord16 byteOrder $ tagId "SamplesPerPixel"
        putWord16 byteOrder $ typeId "Short"
        putWord32 byteOrder 1
        putWord32 byteOrder . fromIntegral $ samplesPerPixel info
    in (offset + 12, output)

  stripByteCountsChunk offset =
    let output = runPut $ do
        let count = length $ stripByteCounts info
        putWord16 byteOrder $ tagId "StripByteCounts"
        putWord16 byteOrder $ typeId "Long"
        putWord32 byteOrder $ fromIntegral count
        if 1 == count
          then do
            putWord32 byteOrder . head $ stripByteCounts info
          else do
            putWord32 byteOrder $ offset + 12
            mapM_ (putWord32 byteOrder) $ stripByteCounts info
    in (offset + fromIntegral (BSL.length output), output)

  stripOffsetsChunk offset =
    let output = runPut $ do
        let count = length $ stripOffsets info
        putWord16 byteOrder $ tagId "StripOffsets"
        putWord16 byteOrder $ typeId "Long"
        putWord32 byteOrder $ fromIntegral count
        if 1 == count
          then do
            putWord32 byteOrder . head $ stripOffsets info
          else do
            putWord32 byteOrder $ offset + 12
            mapM_ (putWord32 byteOrder) $ stripOffsets info
    in (offset + fromIntegral (BSL.length output), output)

  xResolutionChunk offset =
    let output = runPut $ do
        putWord16 byteOrder $ tagId "XResolution"
        putWord16 byteOrder $ typeId "Rational"
        putWord32 byteOrder 1
        putWord32 byteOrder $ offset + 12
        putWord32 byteOrder . R.numerator $ xResolution info
        putWord32 byteOrder . R.denominator $ xResolution info
    in (offset + 20, output)

  yResolutionChunk offset =
    let output = runPut $ do
        putWord16 byteOrder $ tagId "YResolution"
        putWord16 byteOrder $ typeId "Rational"
        putWord32 byteOrder 1
        putWord32 byteOrder $ offset + 12
        putWord32 byteOrder . R.numerator $ yResolution info
        putWord32 byteOrder . R.denominator $ yResolution info
    in (offset + 20, output)

