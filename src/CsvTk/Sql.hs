module CsvTk.Sql where

import CsvTk.Csv

import Control.Applicative ((<$>), (<*>), (<*), (*>))
import Data.Function (on)
import Data.List (find, findIndices)
import Data.Maybe (fromMaybe)
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Text.Parsec hiding (State)
import qualified Text.Parsec.Token as T
import Text.Parsec.Language

import Debug.Trace

--
-- Query monad
--

type Query = ErrorT String (StateT SqlState (Reader [Csv]))

data SqlState = MkSqlState
  { sqlNextId :: Int
  }

defaultSqlState = MkSqlState
  { sqlNextId = 0
  }

runQuery q csvs = runReader (evalStateT (runErrorT q) defaultSqlState) csvs


--
-- lexer definition
--

lexer = T.makeTokenParser emptyDef
  { T.commentLine     = "--"
  , T.caseSensitive   = False
  , T.reservedNames   = ["select", "from", "as", "where"]
  }

lexeme = T.lexeme lexer
identifier = T.identifier lexer
reserved = T.reserved lexer
comma = T.comma lexer
dot = T.dot lexer
whiteSpace = T.whiteSpace lexer

--
-- parser definition
--

parseSql :: String -> Either ParseError (Query Csv)
parseSql = parse query "(unknown)"

query = selectQuery

selectQuery = do
  whiteSpace
  lexeme $ reserved "select"
  columns <- lexeme colName `sepBy` comma
  lexeme $ reserved "from"
  tables <- lexeme tableName `sepBy` comma

  return $ do
    csv <- sqlProduct tables
    sqlProject columns csv

  where
    colName = try qualColName <|> unqualColName
    qualColName = (,) <$> (Just <$> identifier <* dot) <*> (identifier <|> string "*")
    unqualColName = (,) Nothing <$> (identifier <|> string "*")

    tableName = (,) <$> identifier
                    <*> (optionMaybe $ optional (lexeme $ reserved "as") *> lexeme identifier)

--
-- relational algebra operators
--

sqlProject :: [(Maybe String, String)] -> Csv -> Query Csv
sqlProject columnNames csv = do
  st <- get
  put st { sqlNextId = 1 + sqlNextId st }

  indices <- forM columnNames $ \column@(t, c) ->
              case findIndices (columnEquals column) (csvHeaders csv) of
                [] -> throwError $ "unknown column: " ++ showCol column
                [i] -> return [i]
                _ -> throwError $ "ambiguous column specification: " ++ showCol column

  let selectors = (map (\i -> (!!i)) $ concat indices) in
    return MkCsv
      { csvName = "#" ++ show (sqlNextId st)
      , csvHeaders = selectors `ap` [csvHeaders csv]
      , csvCells = map (ap selectors . return) $ csvCells csv
      }

  where
    columnEquals (Nothing, col1) (table2, col2)     = col1 == col2
    columnEquals (Just table1, col1) (table2, col2) = col1 == col2 && table1 == table2

    showCol (Nothing, column) = column
    showCol (Just table, column) = table ++ "." ++ column


sqlSelect :: Csv -> Query Csv
sqlSelect csv = undefined


sqlRename :: Csv -> Query Csv
sqlRename csv = undefined


sqlProduct :: [(String, Maybe String)] -> Query Csv
sqlProduct tableNames = do
  st <- get
  put st { sqlNextId = 1 + sqlNextId st }

  allTables <- ask

  csvs <- forM tableNames $ \(name, alias) ->
            case find (\c -> (csvName c == name)) allTables of
              Nothing -> throwError $ "table `" ++ name ++ "' not found"
              Just csv -> return csv { csvName = fromMaybe (csvName csv) alias }

  return MkCsv
    { csvName = "#" ++ show (sqlNextId st)
    , csvHeaders = concatMap qualifiedHeaders csvs
    , csvCells = foldl1 (\a b -> (++) <$> a <*> b) (map csvCells csvs)
    }

  where
    qualifiedHeaders csv = map qualifyHeader $ csvHeaders csv
      where qualifyHeader (_, header) = (csvName csv, header)


