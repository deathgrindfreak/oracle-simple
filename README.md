oracle-simple
=====================================

Modern bindings to Oracle [odpic](https://oracle.github.io/odpi/) C library.
 - See [here](https://github.com/oracle/odpi/blob/main/include/dpi.h) for a list of all structs and functions used in this library.

## Example

```haskell
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Data.Text (Text)
import Database.Oracle.Simple
import GHC.Generics (Generic)

main :: IO ()
main = do
  let stmt = "select count(*), sysdate, 'ignore next column', 125.24, 3.14 from dual"
  conn <- createConn (ConnectionParams "username" "password" "localhost/XEPDB1")
  rows <- query @ReturnedRow conn stmt
  print rows

-- [ ReturnedRow { count = RowCount {getRowCount = 1.0}
--               , sysdate = DPITimeStamp {year = 2023, month = 9, day = 15, hour = 2, minute = 10, second = 50, fsecond = 0, tzHourOffset = 0, tzMinuteOffset = 0}
--               , hint = "ignore next column"
--               , amount = 125.24000000000001
--               , piValue = 3.14
--               }
-- ]

newtype RowCount = RowCount { getRowCount :: Double }
  deriving (Show)

instance FromField RowCount where
  fromField = RowCount <$> fromField

data ReturnedRow = ReturnedRow
  { count :: RowCount
  , sysdate :: DPITimeStamp
  , hint :: Text
  , amount :: Double
  , piValue :: Double
  }
  deriving stock (Show, Generic)
  deriving anyclass FromRow

-- instance FromRow ReturnedRow where
--   fromRow = do
--     count <- field
--     sysdate <- field
--     amount <- field
--     pure ReturnedRow{..}

```

## Subscriptions
CQN (Continuous Query Notifications) are supported.  The following code shows how to set up subscriptions with a callback:

``` haskell
subscribe = runOracle $ do
  let
    cb msg = do
      MIO.liftIO $ putStrLn "!!!RECEIVED MESSAGE!!!"
      MIO.liftIO $ print msg

    opts =
      Just 
        OS.SubscriptionCreateOptions
          { OS.operations = OS.AllNotifications
          , OS.qos = Set.fromList [OS.QOSQuery, OS.QOSRowIds]
          }

  MIO.liftIO $ print "Subscribing ..."
  OS.withSubscription opts cb $ do
    MIO.liftIO $ print "Registering ..."
    OS.register "SELECT * FROM test_table"

  forever $ do
    MIO.liftIO $ print "sleeping ..."
    UnliftIO.threadDelay $ 5 * 1000 * 1000
    
runQueries = runOracle $ do
  void $ OS.execute_ "create table if not exists test_table (num_column number(10,0) primary key)"

  void $ OS.execute_ "delete from test_table"
  for_ [1 .. 10] $ \i -> do
    void $ OS.execute "insert into test_table values (:1)" (OS.Only @Int i)

  rs <- coerce @[OS.Only Int] @[Int] <$> OS.query_ "select * from test_table"
  MIO.liftIO $ print rs
```

Running `subscribe` first and then `runQueries` in another repl (while having an Oracle database running) will produce something like the following: 

``` text
!!!RECEIVED MESSAGE!!!
SubscriberMessage {eventType = EventQueryChange, dbName = "ORACLEDB", tables = [], queries = [SubscriptionMessageQuery {queryID = 2, queryOperations = Notifications (fromList [AllRowsNotification,InsertNotification,UpdateNotification]), queryTables = [SubscriptionMessageTable {tableOperations = Notifications (fromList [DeleteNotification]), tableName = "EMD.TEST_TABLE", tableRows = [SubscriptionMessageRow {rowOperations = Notifications (fromList [DeleteNotification]), rowId = "AAARx/AAcAAAAALAAA"},SubscriptionMessageRow {rowOperations = Notifications (fromList [DeleteNotification]), rowId = "AAARx/AAcAAAAALAAB"},SubscriptionMessageRow {rowOperations = Notifications (fromList [DeleteNotification]), rowId = "AAARx/AAcAAAAALAAC"},SubscriptionMessageRow {rowOperations = Notifications (fromList [DeleteNotification]), rowId = "AAARx/AAcAAAAALAAD"},SubscriptionMessageRow {rowOperations = Notifications (fromList [DeleteNotification]), rowId = "AAARx/AAcAAAAALAAE"},SubscriptionMessageRow {rowOperations = Notifications (fromList [DeleteNotification]), rowId = "AAARx/AAcAAAAALAAF"},SubscriptionMessageRow {rowOperations = Notifications (fromList [DeleteNotification]), rowId = "AAARx/AAcAAAAALAAG"},SubscriptionMessageRow {rowOperations = Notifications (fromList [DeleteNotification]), rowId = "AAARx/AAcAAAAALAAH"},SubscriptionMessageRow {rowOperations = Notifications (fromList [DeleteNotification]), rowId = "AAARx/AAcAAAAALAAK"},SubscriptionMessageRow {rowOperations = Notifications (fromList [DeleteNotification]), rowId = "AAARx/AAcAAAAALAAL"}]}]}], errorInfo = Nothing, registered = True, queueName = "", consumerName = ""}
!!!RECEIVED MESSAGE!!!
SubscriberMessage {eventType = EventQueryChange, dbName = "ORACLEDB", tables = [], queries = [SubscriptionMessageQuery {queryID = 2, queryOperations = Notifications (fromList [AllRowsNotification,InsertNotification,UpdateNotification]), queryTables = [SubscriptionMessageTable {tableOperations = Notifications (fromList [InsertNotification]), tableName = "EMD.TEST_TABLE", tableRows = [SubscriptionMessageRow {rowOperations = Notifications (fromList [InsertNotification]), rowId = "AAARx/AAcAAAAALAAI"}]}]}], errorInfo = Nothing, registered = True, queueName = "", consumerName = ""}
...
```

## Developing locally

### Building

Run `./scripts/bootstrap.sh` which will:
* Configure the local `.env` file
* Bring up some docker volumes needed to hold stack data

The build script will build the project without running the tests themselves
```bash
./scripts/build
```

### Running tests

You can run the functional tests with this script
``` bash
./scripts/run_tests.sh
```

##### Running tests in Mac OSX (Apple Silicon Chips)

In order to run the tests on a newer Mac machine, you'll have to use [Colima](https://github.com/abiosoft/colima)

To install via Homebrew, run

``` bash
brew install colima
```

These are the settings I typically use:

``` bash
colima start \
  --arch x86_64 \
  --vm-type=vz \
  --vz-rosetta \
  --mount-type virtiofs \
  --memory 24 \
  --cpu 8
```

If your machine doesn't support Rosetta, this might work instead:

``` bash
colima start \
  --arch x86_64 \
  --memory 24 \
  --cpu 8
```
