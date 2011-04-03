module AutoGRef.Chip
  --(
  --)
  where

import Data.Array
import AutoGRef.Tiff
import AutoGRef.TiffInfo
import AutoGRef.Pixel

--newtype Chip = Chip { fromChip :: Array Int Pixel }

--chip5x5 = array (1, 5) [()]

data Chip = Chip {
    chipWidth :: Int
  , chipData :: Array Int Pixel
}
  deriving (Show)

(!!!) :: Chip -> (Int, Int) -> Pixel
(!!!) chip (r, c) =
  let d = chipData chip; w = chipWidth chip
  in d ! (r * w + c)

