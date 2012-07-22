module CsvTk where

import CsvTk.Parse
import CsvTk.Tabulate

import System.Console.GetOpt
import System.Environment (getArgs)

data ProgramOptions = MkProgramOptions
  { optHelp :: Bool
  , optTabulate :: Bool
  , optCompress :: Bool
  , optFormatStr :: String
  }
  deriving (Eq, Show)

defaultProgramOptions = MkProgramOptions
  { optHelp = False
  , optTabulate = False
  , optCompress = False
  , optFormatStr = ""
  } 

options =
  [ Option "h" ["help"]     (NoArg $ \opt -> opt { optHelp = True })
                            "show help" 
  , Option "t" ["tabulate"] (NoArg $ \opt -> opt { optTabulate = True })
                            "tabulate CSV"
  , Option "c" ["compress"] (NoArg $ \opt -> opt { optCompress = True })
                            "compress CSV by eliminating empty columns"
  , Option "f" ["format"]   (ReqArg (\str opt -> opt { optFormatStr = str }) "FORMAT_STRING")
                            "output columns in specified format"
  ]

main = do
  args <- getArgs
  let (flags, positionals, msgs) = getOpt Permute options args
  let programOptions = foldr ($) defaultProgramOptions flags

  if null msgs
  then do
    print programOptions
  else
    mapM_ putStrLn msgs

