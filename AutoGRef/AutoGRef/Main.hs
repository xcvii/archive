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

fileName =
  "/home/baja/Downloads/elte_tfe_feladatok_2_bement/nem_referalt/0506-8bit.tif"

main = do
  tiff <- decodeFile fileName

  print . hex . runPut $ put (tiff :: Tiff)

