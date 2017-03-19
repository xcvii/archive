module AutoGRef.Main where

import System.IO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Control.Monad

import AutoGRef.Tiff
import AutoGRef.TiffInfo
import AutoGRef.Chip

import Data.Hex

fileName = "/home/baja/Downloads/elte_tfe_feladatok_2_bement/nem_referalt/0506-8bit.tif"

main = do
  tiff <- decodeFile fileName :: IO Tiff

  forM_ [0..10] $ \i -> do
    let chip = getChip (10, 10) tiff (10 * i, 0)
    print (i, chipIntMean chip, chipIntStdDev chip, chipHueMean chip)

