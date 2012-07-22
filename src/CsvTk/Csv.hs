module CsvTk.Csv where

import Data.List (intersperse)
import Data.Maybe (fromMaybe)

data Csv = MkCsv
  { csvName    :: String
  , csvHeaders :: [(String, String)]
  , csvCells   :: [[String]]
  }
  deriving (Show)

headers csv = do
  (tab, col) <- hs
  
  if 1 < (length . filter (== col) $ map snd hs)
  then return $ tab ++ "." ++ col
  else return col

  where hs = csvHeaders csv

pp csv = init . unlines . map (concat . intersperse "," . map quote) $ headers csv : csvCells csv
  where
    quote str = if ',' `elem` str || '"' `elem` str
                then "\"" ++ (escapeQuotes str) ++ "\""
                else str

    escapeQuotes (x:xs)
      | x == '"'  = "\"\"" ++ (escapeQuotes xs)
      | otherwise = x : escapeQuotes xs
    escapeQuotes [] = []

columnCount :: Csv -> Int
columnCount = length . csvCells

rowCount :: Csv -> Int
rowCount = minimum . map length . csvCells

