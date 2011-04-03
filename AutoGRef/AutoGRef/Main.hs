module AutoGRef.Main where

import System.IO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import AutoGRef.Tiff
import AutoGRef.TiffInfo

import Data.Hex

fileName = "/home/baja/Documents/1x800 white rect.tif"

main = do
  tiff <- decodeFile fileName
  encodeFile "a.tif" (tiff :: Tiff)

