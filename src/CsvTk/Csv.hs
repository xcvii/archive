module CsvTk.Csv where

type Csv = [[String]]

columnCount :: Csv -> Int
columnCount = length

rowCount :: Csv -> Int
rowCount = minimum . map length

