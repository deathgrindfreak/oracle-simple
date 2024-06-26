{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main
  ( main,
  )
where

import qualified Control.Exception as Exc
import Control.Monad (void, (<=<))
import qualified Data.Aeson as Aeson
import qualified Data.Time as Time
import qualified Data.Time.Calendar.OrdinalDate as OrdinalDate
import qualified GHC.Generics as Generics
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec (Spec, around, describe, hspec, it, shouldBe)
import Test.Hspec.Hedgehog (hedgehog)

import Database.Oracle.Simple

data SumType = This | That
  deriving (Generics.Generic, Eq, Show)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

data JsonData = JsonData
  { string :: String
  , number :: Int
  , bool :: Bool
  , maybeBool :: Maybe Bool
  , stringList :: [String]
  , sumType :: SumType
  , double :: Double
  }
  deriving (Generics.Generic, Eq, Show)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)
  deriving (FromField, ToField) via AesonField JsonData

data MixTable = MixTable
  { intColumn :: Int
  , jsonColumn :: JsonData
  }
  deriving (Generics.Generic, Eq, Show)
  deriving anyclass (FromRow, ToRow)

main :: IO ()
main = withPool params $ hspec . spec

params :: ConnectionParams
params = ConnectionParams "username" "password" "localhost:1521/devdb" Nothing

genDPITimestamp :: HH.Gen DPITimestamp
genDPITimestamp = do
  let choose (l, h) = Gen.integral (Range.linear l h)
  tzHourOffset <- choose (-14, 14)
  DPITimestamp
    <$> choose (1000, 2023)
    <*> choose (1, 12)
    <*> choose (1, 28)
    <*> choose (1, 23)
    <*> choose (1, 59)
    <*> choose (1, 59)
    <*> choose (0, 100000)
    <*> pure tzHourOffset
    <*> if signum tzHourOffset < 0
      then choose (-59, 0)
      else choose (0, 59)

spec :: Pool -> Spec
spec pool = do
  around (withPoolConnection pool) $ do
    describe "SELECT tests" $ do
      it "Should select timestamp from Oracle" $ \conn -> do
        currentDay <- Time.utctDay <$> Time.getCurrentTime
        [Only DPITimestamp {..}] <- query_ conn "select sysdate from dual"
        currentDay
          `shouldBe` Time.fromGregorian
            (fromIntegral year)
            (fromIntegral month)
            (fromIntegral day)

    describe "Connection tests" $ do
      it "Should check connection health" $ (`shouldBe` True) <=< isHealthy

      it "Should ping connection" $ (`shouldBe` True) <=< ping

    describe "DPITimeStamp tests" $ do
      it "Should roundtrip DPITimestamp through UTCTime" $ \_ -> do
        hedgehog $ do
          dpiTimestamp <- HH.forAll $ genDPITimestamp
          utcTimeToDPITimestamp (dpiTimeStampToUTCTime dpiTimestamp)
            === dpiTimeStampToUTCDPITimeStamp dpiTimestamp

      it "Idempotency of dpiTimeStampToUTCDPITimeStamp " $ \_ -> do
        hedgehog $ do
          dpi <- HH.forAll $ genDPITimestamp
          dpiTimeStampToUTCDPITimeStamp (dpiTimeStampToUTCDPITimeStamp dpi)
            === dpiTimeStampToUTCDPITimeStamp dpi

      it "YYYY/MM/DD should be affected by UTC offset changes" $ \_ -> do
        let dpi =
              DPITimestamp
                { year = 1000
                , month = 1
                , day = 1
                , hour = 0
                , minute = 0
                , second = 0
                , fsecond = 0
                , tzHourOffset = 0
                , tzMinuteOffset = 1
                }
        let expected =
              DPITimestamp
                { year = 999
                , month = 12
                , day = 31
                , hour = 23
                , minute = 59
                , second = 0
                , fsecond = 0
                , tzHourOffset = 0
                , tzMinuteOffset = 0
                }
        dpiTimeStampToUTCDPITimeStamp dpi `shouldBe` expected

      it "Should roundtrip UTCTime through DPITimestamp (w/ nanos -- not picos) " $ \_ -> do
        hedgehog $ do
          utc <- HH.forAll $ do
            d <-
              OrdinalDate.fromOrdinalDate
                <$> Gen.integral (Range.linear 2000 2400)
                <*> Gen.int (Range.linear 1 365)
            seconds <- Gen.integral (Range.linear 0 86400)
            pure $ Time.UTCTime d (Time.secondsToDiffTime seconds)

          utc === dpiTimeStampToUTCTime (utcTimeToDPITimestamp utc)

    describe "JSON tests" $ do
      it "should roundtrip JSON data" $ \conn -> do
        _ <- execute_ conn "create table json_test(test_column json)"
        let jsonData = JsonData "str" 123 True Nothing ["hello", "world"] That 3.14
        _ <- execute conn "insert into json_test values (:1)" (Only jsonData)
        [Only gotData] <- query_ conn "select * from json_test"
        _ <- execute_ conn "drop table json_test"
        gotData `shouldBe` jsonData

      it "handles a mix of json and non-json fields in tables" $ \conn -> do
        _ <- execute_ conn "create table json_mix_test(int_column number(10,0), json_column json)"
        let insertRows =
              [ MixTable 1 (JsonData "str" 123 True Nothing ["hello", "world"] That 3.14)
              , MixTable 2 (JsonData "foo" 456 False (Just False) ["goodbye!"] This 9.99)
              ]
        _ <- executeMany conn "insert into json_mix_test values (:1, :2)" insertRows
        gotRows <- query_ conn "select * from json_mix_test"
        _ <- execute_ conn "drop table json_mix_test"
        gotRows `shouldBe` insertRows

    describe "transaction tests" $ do
      it "should commit transaction successfully" $ \conn -> do
        void $ execute_ conn "create table transaction_test(text_column number(10,0) primary key)"
        void $ withTransaction conn $ do
          void $ execute conn "insert into transaction_test values(:1)" (Only @Int 1)
          void $ execute conn "insert into transaction_test values(:1)" (Only @Int 2)
          void $ execute conn "insert into transaction_test values(:1)" (Only @Int 3)
          void $ execute conn "insert into transaction_test values(:1)" (Only @Int 4)
        results <- query_ @(Only Int) conn "select * from transaction_test"
        void $ execute_ conn "drop table transaction_test"
        results `shouldBe` [Only 1, Only 2, Only 3, Only 4]

      it "should roll back transaction in case of failure" $ \conn -> do
        void $ execute_ conn "create table rollback_test(text_column number(10,0) primary key)"
        handleOracleError $ withTransaction conn $ do
          void $ execute conn "insert into rollback_test values(:1)" (Only @Int 1)
          void $ execute conn "insert into rollback_test values(:1)" (Only @Int 2)
          void $ execute conn "insert into rollback_test values(:1)" (Only @Int 3)
          void $ execute conn "insert into rollback_test values(:1)" (Only @Int 3) -- should fail
        results <- query_ @(Only Int) conn "select * from rollback_test"
        void $ execute_ conn "drop table rollback_test"
        results `shouldBe` [] -- should roll back transaction
      it "should roll back to savepoint" $ \conn -> do
        void $ execute_ conn "create table savepoint_test(text_column number(10,0) primary key)"
        void $ withTransaction conn $ do
          void $ execute conn "insert into savepoint_test values(:1)" (Only @Int 1)
          void $ execute conn "insert into savepoint_test values(:1)" (Only @Int 2)
          handleOracleError $ withSavepoint conn $ do
            void $ execute conn "insert into savepoint_test values(:1)" (Only @Int 3)
            void $ execute conn "insert into savepoint_test values(:1)" (Only @Int 4)
            void $ execute conn "insert into savepoint_test values(:1)" (Only @Int 4) -- should fail
        results <- query_ @(Only Int) conn "select * from savepoint_test"
        void $ execute_ conn "drop table savepoint_test"
        results `shouldBe` [Only 1, Only 2] -- should roll back to before savepoint
      it "allows for nesting savepoints" $ \conn -> do
        void $ execute_ conn "create table savepoint_nesting_test(text_column number(10,0) primary key)"
        withTransaction conn $ do
          void $ execute conn "insert into savepoint_nesting_test values(:1)" (Only @Int 1)
          void $ execute conn "insert into savepoint_nesting_test values(:1)" (Only @Int 2)
          handleOracleError $ withSavepoint conn $ do
            void $ execute conn "insert into savepoint_nesting_test values(:1)" (Only @Int 3)
            void $ execute conn "insert into savepoint_nesting_test values(:1)" (Only @Int 4)
            handleOracleError $ withSavepoint conn $ do
              void $ execute conn "insert into savepoint_nesting_test values(:1)" (Only @Int 5)
              void $ execute conn "insert into savepoint_nesting_test values(:1)" (Only @Int 5) -- should fail
            void $ execute conn "insert into savepoint_nesting_test values(:1)" (Only @Int 6)
        results <- query_ @(Only Int) conn "select * from savepoint_nesting_test"
        void $ execute_ conn "drop table savepoint_nesting_test"
        results `shouldBe` [Only 1, Only 2, Only 3, Only 4, Only 6] -- should roll back to inner savepoint
      it "handles consecutive transactions" $ \conn -> do
        void $ execute_ conn "create table transactions_test(text_column number(10,0) primary key)"
        -- transaction that inserts rows
        void $ withTransaction conn $ do
          void $ execute conn "insert into transactions_test values(:1)" (Only @Int 1)
          void $ execute conn "insert into transactions_test values(:1)" (Only @Int 2)
          void $ execute conn "insert into transactions_test values(:1)" (Only @Int 3)
          void $ execute conn "insert into transactions_test values(:1)" (Only @Int 4)
        -- transaction that makes no changes that require commit
        void $ withTransaction conn $ do
          _ <- query_ @(Only Int) conn "select * from transactions_test"
          pure ()
        -- transaction that inserts rows with savepoint
        void $ withTransaction conn $ do
          void $ execute conn "insert into transactions_test values(:1)" (Only @Int 5)
          void $ withSavepoint conn $ do
            void $ execute conn "insert into transactions_test values(:1)" (Only @Int 6)
          void $ execute conn "insert into transactions_test values(:1)" (Only @Int 7)
        -- transaction that is rolled back
        handleOracleError $ withTransaction conn $ do
          void $ execute conn "insert into transactions_test values(:1)" (Only @Int 6) -- should fail
          -- transaction that inserts rows with savepoint that is rolled back to
        void $ withTransaction conn $ do
          void $ execute conn "insert into transactions_test values(:1)" (Only @Int 8)
          handleOracleError $ withSavepoint conn $ do
            void $ execute conn "insert into transactions_test values(:1)" (Only @Int 9)
            void $ execute conn "insert into transactions_test values(:1)" (Only @Int 9) -- should fail
          void $ execute conn "insert into transactions_test values(:1)" (Only @Int 10)
        results <- query_ @(Only Int) conn "select * from transactions_test"
        void $ execute_ conn "drop table transactions_test"
        results `shouldBe` [Only 1 .. Only 8] <> [Only 10]
  where
    handleOracleError action = Exc.try @OracleError action >>= either (\_ -> pure ()) (\_ -> pure ())
