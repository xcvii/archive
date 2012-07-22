module Test where

import CsvTk.Csv
import CsvTk.Sql

import Control.Monad.Error
import Control.Monad.Reader

import Data.List (find)

fooCsv = MkCsv
  { csvName = "foo"
  , csvHeaders = map ((,) "foo") ["id", "h1", "h2"]
  , csvCells = [["1", "foo", "bar"]]
  }

xyzzyCsv = MkCsv
  { csvName = "xyzzy"
  , csvHeaders = map ((,) "xyzzy") ["id", "h3", "h4"]
  , csvCells = [["1", "xyzzy", "plugh"]
               ,["2", "foo,bar", "\"quux\""]]
  }

selectFoo = "SELECT h1, f.h2, h2 FROM foo AS f"

csvs = [fooCsv, xyzzyCsv]

cols = [(Nothing, "h4"), (Just "xyzzy", "id"), (Nothing, "h4")]

