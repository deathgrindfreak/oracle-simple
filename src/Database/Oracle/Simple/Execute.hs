{-# LANGUAGE BangPatterns #-}

module Database.Oracle.Simple.Execute
  ( execute,
    execute_,
    executeMany,
  )
where

import Control.Monad (foldM)
import qualified Control.Monad.IO.Class as MIO
import Control.Monad.State.Strict (evalStateT)
import Data.Word (Word64)

import Database.Oracle.Simple.Internal (Column (Column), dpiExecute, getRowCount, prepareStmt)
import Database.Oracle.Simple.Monad (MonadOracle, getExecutionMode, withOracleConnection)
import Database.Oracle.Simple.ToRow (RowWriter (runRowWriter), ToRow, toRow)

{- | Execute an INSERT, UPDATE, or other SQL query that is not expected to return results.
Returns the number of rows affected.
-}
execute :: (ToRow a, MonadOracle m) => String -> a -> m Word64
execute sql param = do
  mode <- getExecutionMode
  withOracleConnection $ \conn -> MIO.liftIO $ do
    stmt <- prepareStmt conn sql
    _ <- evalStateT (runRowWriter (toRow param) stmt) (Column 0)
    _ <- dpiExecute stmt mode
    getRowCount stmt

-- | A version of 'execute' that does not perform query substitution.
execute_ :: MonadOracle m => String -> m Word64
execute_ sql = do
  mode <- getExecutionMode
  withOracleConnection $ \conn -> MIO.liftIO $ do
    stmt <- prepareStmt conn sql
    _ <- dpiExecute stmt mode
    getRowCount stmt

{- | Execute a multi-row INSERT, UPDATE or other SQL query that is not expected to return results.
Returns the number of rows affected. If the list of parameters is empty, the function will simply
return 0 without issuing the query to the backend.
-}
executeMany :: (ToRow a, MonadOracle m) => String -> [a] -> m Word64
executeMany _ [] = pure 0
executeMany sql params = do
  mode <- getExecutionMode
  let
    go stmt !totalRowsAffected param = do
      _ <- evalStateT (runRowWriter (toRow param) stmt) (Column 0)
      _ <- dpiExecute stmt mode
      rowsAffected <- getRowCount stmt
      pure (totalRowsAffected + rowsAffected)
  withOracleConnection $ \conn -> MIO.liftIO $ do
    stmt <- prepareStmt conn sql
    foldM (go stmt) 0 params
