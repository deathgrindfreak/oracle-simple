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
