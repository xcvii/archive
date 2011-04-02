module AutoGRef.Main where

import System.IO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Binary
import Data.Binary.Get

import AutoGRef.Tiff

fileName =
  "/home/baja/Downloads/elte_tfe_feladatok_2_bement/nem_referalt/0506-8bit.tif"

main = do
  handle <- openBinaryFile fileName ReadMode

  buffer <- BSL.hGetContents handle

  let tiff = runGet (get :: Get Tiff) buffer

  print tiff

  hClose handle

