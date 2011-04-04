module AutoGRef.Chip
  --(
  --)
  where

import Data.Array
import AutoGRef.Tiff
import AutoGRef.TiffInfo
import AutoGRef.Pixel

data Chip = Chip {
    chipWidth :: Int
  , chipData :: Array Int Pixel
}
  deriving (Show)

(!!!) :: Chip -> (Int, Int) -> Pixel
(!!!) chip (r, c) =
  let d = chipData chip; w = chipWidth chip
  in d ! (r * w + c)

