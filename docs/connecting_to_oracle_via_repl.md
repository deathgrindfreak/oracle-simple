# Connecting and Testing via the REPL

Running the tests over and over again can work fine in general, but sometimes it'd be nice if the iteration cycle was a bit tighter.  This is why I've enabled the stack repl to have the ability to connect to a local (or I suppose non-local) Oracle database instance.

## Testing locally

First start up the local Oracle development database
```sh
docker compose -f docker-compose.test.yml up dev-db
```

In a separate terminal spin up the repl (you won't be able to run Oracle commands until the Oracle gives the "DATABASE IS READY TO USE!" output)
```sh
./scripts/repl.sh
```

In order to connect with the locally running db, we'll need to use the following `ConnectionParams`, where the `host.docker.internal` allows a docker container (in this instance the repl) to connect to a localhost (in this case the db, since it forwards the 1521 port).
```haskell
ConnectionParams "username" "password" "host.docker.internal:1521/devdb"
```

Now you can just run the tests locally, refreshing the repl via `:r` as needed
```sh 
repl> :l Main
repl> main
```

I've also included a Scratch.hs example file that can be used to do some additional commands that are helpful in my humble opinion (note that I've included this in the gitignore if you put it in the top-level directory).
```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Scratch
  ( OracleDev,
    runOracle,
    dropAllTables,
    dropConcurrencyTables,
    createConcurrencyTable,
    resetConcurrencyTable,
    selectAllConcurrencyTable,
    runConcurrentInserts,
    runConcurrentInsertsBad,
    createSavePointTable,
    savePointTest,
  )
where

import Control.Exception.Safe (MonadCatch, MonadMask, MonadThrow, try, tryAny)
import Control.Monad (void)
import qualified Control.Monad.Fail as Fail
import qualified Control.Monad.IO.Class as MIO
import Control.Monad.Trans.Reader (ReaderT (..), ask, local)
import Data.Coerce (coerce)
import qualified Database.Oracle.Simple as OS
import qualified UnliftIO
import qualified UnliftIO.Async as Async

newtype OracleDev a = OracleDev
  { runOracleDev :: ReaderT OS.OracleEnv IO a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MIO.MonadIO
    , MonadCatch
    , MonadMask
    , MonadThrow
    , Fail.MonadFail
    , OS.MonadOracle
    , UnliftIO.MonadUnliftIO
    )

instance OS.HasOracleContext OracleDev where
  getOracleEnv = OracleDev ask
  localOracleEnv f = OracleDev . local f . runOracleDev

params :: OS.ConnectionParams
params =
  OS.ConnectionParams "username" "password" "host.docker.internal:1521/devdb" $
    Just $
      OS.defaultAdditionalConnectionParams
        { OS.minSessions = 1
        , OS.maxSessions = 1000
        , OS.sessionIncrement = 1
        , OS.timeout = 30
        }

runOracle :: OracleDev a -> IO a
runOracle action =
  OS.withPool params $ \pool ->
    runReaderT (runOracleDev action) (OS.newOracleEnv pool)

dropAllTables :: IO ()
dropAllTables = runOracle $ do
  void . tryAny $ OS.execute_ "drop table json_test"
  void . tryAny $ OS.execute_ "drop table json_mix_test"
  void . tryAny $ OS.execute_ "drop table task_test"
  void . tryAny $ OS.execute_ "drop table transaction_test"
  void . tryAny $ OS.execute_ "drop table rollback_test"
  void . tryAny $ OS.execute_ "drop table savepoint_test"
  void . tryAny $ OS.execute_ "drop table savepoint_nesting_test"
  void . tryAny $ OS.execute_ "drop table transactions_test"
  void . tryAny $ OS.execute_ "drop table concurrency_test_no_tx"
  void . tryAny $ OS.execute_ "drop table concurrency_test_query"
  void . tryAny $ OS.execute_ "drop table concurrency_test"
  void . tryAny $ OS.execute_ "drop table concurrency_test_inner"
  void . tryAny $ OS.execute_ "drop table concurrency_test_nested"

dropConcurrencyTables :: IO ()
dropConcurrencyTables = runOracle $ do
  void $ OS.execute_ "drop table concurrency_test"
  void $ OS.execute_ "drop table concurrency_test_inner"

createConcurrencyTable :: IO ()
createConcurrencyTable = runOracle $ do
  void $ OS.execute_ "create table concurrency_test (num_column number(10,0) primary key)"

resetConcurrencyTable :: IO ()
resetConcurrencyTable = runOracle $ do
  void $ OS.execute_ "delete from concurrency_test"

selectAllConcurrencyTable :: IO [Int]
selectAllConcurrencyTable = runOracle $ do
  coerce @[OS.Only Int] <$> OS.query_ "select * from concurrency_test"

runConcurrentInserts :: IO [Int]
runConcurrentInserts = runOracle $ do
  let rows = [0, 2 .. 100]
  Async.pooledForConcurrentlyN_ 5 rows $ \i -> do
    OS.withTransaction $ do
      void $ OS.execute "insert into concurrency_test values (:1)" (OS.Only @Int i)
      void $ OS.execute "insert into concurrency_test values (:1)" (OS.Only @Int (i + 1))
  coerce @[OS.Only Int] <$> OS.query_ "select * from concurrency_test"

runConcurrentInsertsBad :: IO [Int]
runConcurrentInsertsBad = runOracle $ do
  let rows = [0, 2 .. 1000]
  OS.withTransaction $ do
    Async.pooledForConcurrentlyN_ 100 rows $ \i -> do
      void $ OS.execute "insert into concurrency_test values (:1)" (OS.Only @Int i)
      void $ OS.execute "insert into concurrency_test values (:1)" (OS.Only @Int (i + 1))
  coerce @[OS.Only Int] <$> OS.query_ "select * from concurrency_test"

createSavePointTable :: IO ()
createSavePointTable = runOracle $ do
  void $ OS.execute_ "create table savepoint_test (num_column number(10,0) primary key)"

savePointTest :: IO [Int]
savePointTest = runOracle $ do
  void $ OS.withTransaction $ do
    void $ OS.execute "insert into savepoint_test values(:1)" (OS.Only @Int 1)
    void $ OS.execute "insert into savepoint_test values(:1)" (OS.Only @Int 2)
    handleOracleError $ OS.withTransaction $ do
      void $ OS.execute "insert into savepoint_test values(:1)" (OS.Only @Int 3)
      void $ OS.execute "insert into savepoint_test values(:1)" (OS.Only @Int 4)
      void $ OS.execute "insert into savepoint_test values(:1)" (OS.Only @Int 4) -- should fail
  coerce @[OS.Only Int] <$> OS.query_ "select * from savepoint_test"

handleOracleError :: OS.MonadOracle m => m () -> m ()
handleOracleError action = do
  rs <- try @_ @OS.OracleError action
  either (const (pure ())) (const (pure ())) rs
```
