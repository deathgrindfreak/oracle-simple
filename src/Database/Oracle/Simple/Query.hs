module Database.Oracle.Simple.Query
  ( query,
    query_,
    forEach_,
    queryOne,
    queryOne_,
    queryOneOrNone,
    queryOneOrNone_,
  )
where

import Control.Exception.Safe (Exception, throwM)
import qualified Control.Monad.IO.Class as MIO
import Control.Monad.State.Strict (evalStateT)

import Database.Oracle.Simple.FromRow (FromRow, getRow)
import Database.Oracle.Simple.Internal (Column (Column), dpiExecute, fetch, prepareStmt)
import Database.Oracle.Simple.Monad (MonadOracle, getExecutionMode, withOracleConnection)
import Database.Oracle.Simple.ToRow (RowWriter (runRowWriter), ToRow, toRow)

{- | Perform a SELECT or other SQL query that is expected to return results.
All results are retrieved and converted before this function ends.
-}
query ::
  (FromRow a, ToRow b, MonadOracle m) =>
  String ->
  b ->
  m [a]
query sql param = do
  let
    loop _ n | n < 1 = pure []
    loop stmt _ = do
      tsVal <- getRow stmt
      found <- fetch stmt
      (tsVal :) <$> loop stmt found
  mode <- getExecutionMode
  withOracleConnection $ \conn -> MIO.liftIO $ do
    stmt <- prepareStmt conn sql
    _ <- evalStateT (runRowWriter (toRow param) stmt) (Column 0)
    _ <- dpiExecute stmt mode
    found <- fetch stmt
    loop stmt found

-- | A version of 'query' that does not perform query substitution.
query_ ::
  (FromRow a, MonadOracle m) =>
  String ->
  m [a]
query_ sql = do
  let
    loop _ n | n < 1 = pure []
    loop stmt _ = do
      tsVal <- getRow stmt
      found <- fetch stmt
      (tsVal :) <$> loop stmt found
  mode <- getExecutionMode
  withOracleConnection $ \conn -> MIO.liftIO $ do
    stmt <- prepareStmt conn sql
    _ <- MIO.liftIO $ dpiExecute stmt mode
    found <- fetch stmt
    loop stmt found

-- Incrementally process a query
forEach_ ::
  (FromRow row, MonadOracle m) =>
  String ->
  (row -> m ()) ->
  m ()
forEach_ sql cont = do
  let
    loop _ n | n < 1 = pure ()
    loop stmt _ = do
      tsVal <- MIO.liftIO $ getRow stmt
      cont tsVal
      found <- MIO.liftIO $ fetch stmt
      loop stmt found
  mode <- getExecutionMode
  withOracleConnection $ \conn -> do
    stmt <- MIO.liftIO $ prepareStmt conn sql
    _ <- MIO.liftIO $ dpiExecute stmt mode
    found <- MIO.liftIO $ fetch stmt
    loop stmt found

queryOneOrNone_ ::
  (FromRow a, MonadOracle m) =>
  String ->
  m (Maybe a)
queryOneOrNone_ sql = do
  rs <- query_ sql
  case rs of
    [] -> pure Nothing
    [r] -> pure $ Just r
    _ -> throwM $ QueryError "Expected one or zero rows"

queryOneOrNone ::
  (FromRow a, ToRow b, MonadOracle m) =>
  String ->
  b ->
  m (Maybe a)
queryOneOrNone sql params = do
  rs <- query sql params
  case rs of
    [] -> pure Nothing
    [r] -> pure $ Just r
    _ -> throwM $ QueryError "Expected one or zero rows"

queryOne_ :: (FromRow a, MonadOracle m) => String -> m a
queryOne_ sql = do
  rs <- query_ sql
  case rs of
    [r] -> pure $ r
    _ -> throwM $ QueryError "Expected one row"

queryOne :: (FromRow a, ToRow b, MonadOracle m) => String -> b -> m a
queryOne sql params = do
  rs <- query sql params
  case rs of
    [r] -> pure $ r
    _ -> throwM $ QueryError "Expected one row"

newtype QueryError = QueryError String
  deriving (Show)

instance Exception QueryError
