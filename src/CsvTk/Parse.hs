module CsvTk.Parse where

import CsvTk.Csv

import Text.ParserCombinators.Parsec

csvFile = line `sepEndBy` eol
line = cell `sepBy` char ','
cell = quotedCell <|> many (noneOf ",\n\r")

quotedCell = do
  char '"'
  content <- many quotedChar
  char '"' <?> "quote at end of cell"
  return content


quotedChar = noneOf "\"" <|> try (string "\"\"" >> return '"')

eol = try (string "\n\r")
  <|> try (string "\r\n")
  <|> string "\n"
  <|> string "\r"
  <?> "end of line"

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input

