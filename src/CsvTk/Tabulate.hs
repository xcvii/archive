module CsvTk.Tabulate where

import CsvTk.Csv

import Control.Monad.Reader

data TabulateConfig = MkTabulateConfig
  { minColumnWidth :: Int
  , columnFormats :: [()]
  }

defaultTabulateConfig = MkTabulateConfig
  { minColumnWidth = 0
  , columnFormats = []
  }

type Tabulate = Reader TabulateConfig

tabulate :: TabulateConfig -> Csv -> String
tabulate = undefined

columnWidths :: Csv -> Tabulate [Int]
columnWidths csv = do
  minWidth <- ask >>= return . minColumnWidth
  return . foldl (zipWith max) (repeat minWidth) . (map $ map length) $ csv

alignCell :: String -> Int -> Int -> Tabulate String
alignCell cell width index = do
  undefined

