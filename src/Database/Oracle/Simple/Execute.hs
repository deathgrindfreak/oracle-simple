{-# LANGUAGE BangPatterns #-}

module Database.Oracle.Simple.Execute
  ( execute,
    execute_,
    executeMany,
    executeManyArray,
  )
where

import Control.Monad (foldM)
import qualified Control.Monad.IO.Class as MIO
import Control.Monad.State.Strict (evalStateT)
import Data.List.NonEmpty (NonEmpty)
import Data.Word (Word64)

import Database.Oracle.Simple.Internal
import Database.Oracle.Simple.Monad (MonadOracle, getExecutionMode, withLockedOracleConnection)
import Database.Oracle.Simple.ToRow (RowWriter (runRowWriter), ToRow, toRow)
import Database.Oracle.Simple.Variable

{- | Execute an INSERT, UPDATE, or other SQL query that is not expected to return results.
Returns the number of rows affected.
-}
execute :: (ToRow a, MonadOracle m) => SqlStatement -> a -> m Word64
execute sql param = do
  mode <- getExecutionMode
  withLockedOracleConnection sql $ \conn -> MIO.liftIO $ do
    stmt <- prepareStmt conn sql
    _ <- evalStateT (runRowWriter (toRow param) stmt) (Column 0)
    _ <- dpiExecute stmt mode
    count <- getRowCount stmt
    closeStatement stmt
    pure count

-- | A version of 'execute' that does not perform query substitution.
execute_ :: MonadOracle m => SqlStatement -> m Word64
execute_ sql = do
  mode <- getExecutionMode
  withLockedOracleConnection sql $ \conn -> MIO.liftIO $ do
    stmt <- prepareStmt conn sql
    _ <- dpiExecute stmt mode
    count <- getRowCount stmt
    closeStatement stmt
    pure count

{- | Execute a multi-row INSERT, UPDATE or other SQL query that is not expected to return results.
Returns the number of rows affected. If the list of parameters is empty, the function will simply
return 0 without issuing the query to the backend.
-}
executeMany :: (ToRow a, MonadOracle m) => SqlStatement -> [a] -> m Word64
executeMany _ [] = pure 0
executeMany sql params = do
  mode <- getExecutionMode
  let
    go stmt !totalRowsAffected param = do
      _ <- evalStateT (runRowWriter (toRow param) stmt) (Column 0)
      _ <- dpiExecute stmt mode
      rowsAffected <- getRowCount stmt
      pure (totalRowsAffected + rowsAffected)
  withLockedOracleConnection sql $ \conn -> MIO.liftIO $ do
    stmt <- prepareStmt conn sql
    rs <- foldM (go stmt) 0 params
    closeStatement stmt
    pure rs

{- | This uses the executeMany function from odpi-c:
https://odpi-c.readthedocs.io/en/latest/functions/dpiStmt.html#c.dpiStmt_executeMany.
It iterates over bound variable arrays and executes the statement for each row/position
in the bindings. This is more performant than multiple individual calls to execute, which
incurs a round-trip cost for each call.

You should prefer using this for bulk inserts/updates.

NOTE: This currently does not support arrays coming OUT, only IN.
-}
executeManyArray ::
  (ToBinding a, MonadOracle m) =>
  SqlStatement ->
  NonEmpty a ->
  m Word64
executeManyArray sql rows = do
  mode <- getExecutionMode
  withLockedOracleConnection sql $ \conn -> MIO.liftIO $ do
    stmt <- prepareStmt conn sql
    totalRows <- bindRows conn stmt rows
    _ <- dpiExecuteMany stmt mode totalRows
    count <- getRowCount stmt
    closeStatement stmt
    pure count
