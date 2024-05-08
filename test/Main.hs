{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main
  ( main,
  )
where

import Control.Exception.Safe (MonadCatch, MonadThrow)
import qualified Control.Exception.Safe as Exc
import Control.Monad (join, void)
import qualified Control.Monad.IO.Class as MIO
import Control.Monad.Trans.Reader (ReaderT (..), ask, local)
import qualified Data.Aeson as Aeson
import Data.Coerce (coerce)
import qualified Data.List as List
import qualified Data.Time as Time
import qualified Data.Time.Calendar.OrdinalDate as OrdinalDate
import qualified GHC.Generics as Generics
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.Hspec.Hedgehog (hedgehog)
import UnliftIO (MonadUnliftIO)
import qualified UnliftIO.Async as Async

import Database.Oracle.Simple
import WaitForOracle (waitForOracle)

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
main = do
  waitForOracle params (2 * 60) 5
  withPool params $ hspec . spec

params :: ConnectionParams
params =
  ConnectionParams "username" "password" "dev-db:1521/devdb" $
    Just $
      defaultAdditionalConnectionParams
        { minSessions = 20
        , sessionIncrement = 10
        , maxSessions = 1000
        , timeout = 5 * 60
        , waitTimeout = 5 * 60 * 1000
        , maxLifetimeSession = 5 * 60
        }

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

newtype TestM a = TestM {runTestM :: ReaderT OracleEnv IO a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MIO.MonadIO
    , MonadOracle
    , MonadThrow
    , MonadCatch
    , MonadFail
    , MonadUnliftIO
    )

instance HasOracleContext TestM where
  getOracleEnv = TestM ask
  localOracleEnv f = TestM . local f . runTestM

-- IO () is nested inside of TestM so that we can return a do block with multiple Expectations
runSpec :: Pool -> TestM (IO ()) -> IO ()
runSpec pool testM =
  join $ runReaderT (runTestM testM) (newOracleEnv pool)

spec :: Pool -> Spec
spec pool = do
  describe "SELECT tests" $ do
    it "Should select timestamp from Oracle" $ runSpec pool $ do
      currentDay <- MIO.liftIO $ fmap Time.utctDay Time.getCurrentTime
      [Only DPITimestamp {..}] <- query_ "select sysdate from dual"
      pure $ do
        currentDay
          `shouldBe` Time.fromGregorian
            (fromIntegral year)
            (fromIntegral month)
            (fromIntegral day)

  describe "Connection tests" $ do
    it "Should check connection health" $ runSpec pool $ do
      healthy <- isHealthy
      pure $ do
        healthy `shouldBe` True

    it "Should ping connection" $ runSpec pool $ do
      p <- ping
      pure $ do
        p `shouldBe` True

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
    it "should roundtrip JSON data" $ runSpec pool $ do
      _ <- execute_ "create table json_test(test_column json)"
      let jsonData = JsonData "str" 123 True Nothing ["hello", "world"] That 3.14
      rowCount <- execute "insert into json_test values (:1)" (Only jsonData)
      [Only gotData] <- query_ "select * from json_test"
      _ <- execute_ "drop table json_test"
      pure $ do
        rowCount `shouldBe` 1
        gotData `shouldBe` jsonData

    it "handles a mix of json and non-json fields in tables" $ runSpec pool $ do
      _ <- execute_ "create table json_mix_test(int_column number(10,0), json_column json)"
      let insertRows =
            [ MixTable 1 (JsonData "str" 123 True Nothing ["hello", "world"] That 3.14)
            , MixTable 2 (JsonData "foo" 456 False (Just False) ["goodbye!"] This 9.99)
            ]
      rowCount <- executeMany "insert into json_mix_test values (:1, :2)" insertRows
      gotRows <- query_ "select * from json_mix_test"
      _ <- execute_ "drop table json_mix_test"
      pure $ do
        rowCount `shouldBe` 2
        gotRows `shouldBe` insertRows

  describe "task tests" $ do
    it "Should be able to run queries inside of a task block" $ runSpec pool $ do
      void $ execute_ "create table task_test(num number(10,0) primary key, txt varchar2(50 char))"
      results <- task $ do
        void $ execute "insert into task_test values(:1, :2)" (1 :: Int, "a")
        void $ execute "insert into task_test values(:1, :2)" (2 :: Int, "b")
        void $ execute "insert into task_test values(:1, :2)" (3 :: Int, "c")
        void $ execute "insert into task_test values(:1, :2)" (4 :: Int, "d")
        query_ @(Int, String) "select * from task_test"
      void $ execute_ "drop table task_test"
      pure $ do
        results `shouldBe` [(1, "a"), (2, "b"), (3, "c"), (4, "d")]

  describe "transaction tests" $ do
    it "should commit transaction successfully" $ runSpec pool $ do
      void $ execute_ "create table transaction_test(text_column number(10,0) primary key)"
      void $ withTransaction $ do
        void $ execute "insert into transaction_test values(:1)" (Only @Int 1)
        void $ execute "insert into transaction_test values(:1)" (Only @Int 2)
        void $ execute "insert into transaction_test values(:1)" (Only @Int 3)
        void $ execute "insert into transaction_test values(:1)" (Only @Int 4)
      results <- query_ @(Only Int) "select * from transaction_test"
      void $ execute_ "drop table transaction_test"
      pure $ do
        results `shouldBe` [Only 1, Only 2, Only 3, Only 4]

    it "should roll back transaction in case of failure" $ runSpec pool $ do
      void $ execute_ "create table rollback_test(text_column number(10,0) primary key)"
      handleOracleError $ withTransaction $ do
        void $ execute "insert into rollback_test values(:1)" (Only @Int 1)
        void $ execute "insert into rollback_test values(:1)" (Only @Int 2)
        void $ execute "insert into rollback_test values(:1)" (Only @Int 3)
        void $ execute "insert into rollback_test values(:1)" (Only @Int 3) -- should fail
      results <- query_ @(Only Int) "select * from rollback_test"
      void $ execute_ "drop table rollback_test"
      pure $ do
        results `shouldBe` [] -- should roll back transaction
    it "should roll back to savepoint" $ runSpec pool $ do
      void $ execute_ "create table savepoint_test(text_column number(10,0) primary key)"
      void $ withTransaction $ do
        void $ execute "insert into savepoint_test values(:1)" (Only @Int 1)
        void $ execute "insert into savepoint_test values(:1)" (Only @Int 2)
        handleOracleError $ withTransaction $ do
          void $ execute "insert into savepoint_test values(:1)" (Only @Int 3)
          void $ execute "insert into savepoint_test values(:1)" (Only @Int 4)
          void $ execute "insert into savepoint_test values(:1)" (Only @Int 4) -- should fail
      results <- query_ @(Only Int) "select * from savepoint_test"
      void $ execute_ "drop table savepoint_test"
      pure $ do
        results `shouldBe` [Only 1, Only 2] -- should roll back to before savepoint
    it "allows for nesting savepoints" $ runSpec pool $ do
      void $ execute_ "create table savepoint_nesting_test(text_column number(10,0) primary key)"
      withTransaction $ do
        void $ execute "insert into savepoint_nesting_test values(:1)" (Only @Int 1)
        void $ execute "insert into savepoint_nesting_test values(:1)" (Only @Int 2)
        handleOracleError $ withTransaction $ do
          void $ execute "insert into savepoint_nesting_test values(:1)" (Only @Int 3)
          void $ execute "insert into savepoint_nesting_test values(:1)" (Only @Int 4)
          handleOracleError $ withTransaction $ do
            void $ execute "insert into savepoint_nesting_test values(:1)" (Only @Int 5)
            void $ execute "insert into savepoint_nesting_test values(:1)" (Only @Int 5) -- should fail
          void $ execute "insert into savepoint_nesting_test values(:1)" (Only @Int 6)
      results <- query_ @(Only Int) "select * from savepoint_nesting_test"
      void $ execute_ "drop table savepoint_nesting_test"
      pure $ do
        results `shouldBe` [Only 1, Only 2, Only 3, Only 4, Only 6] -- should roll back to inner savepoint
    it "handles consecutive transactions" $ runSpec pool $ do
      void $ execute_ "create table transactions_test(text_column number(10,0) primary key)"
      -- transaction that inserts rows
      void $ withTransaction $ do
        void $ execute "insert into transactions_test values(:1)" (Only @Int 1)
        void $ execute "insert into transactions_test values(:1)" (Only @Int 2)
        void $ execute "insert into transactions_test values(:1)" (Only @Int 3)
        void $ execute "insert into transactions_test values(:1)" (Only @Int 4)
      -- transaction that makes no changes that require commit
      void $ withTransaction $ do
        _ <- query_ @(Only Int) "select * from transactions_test"
        pure ()
      -- transaction that inserts rows with savepoint
      void $ withTransaction $ do
        void $ execute "insert into transactions_test values(:1)" (Only @Int 5)
        void $ withTransaction $ do
          void $ execute "insert into transactions_test values(:1)" (Only @Int 6)
        void $ execute "insert into transactions_test values(:1)" (Only @Int 7)
      -- transaction that is rolled back
      handleOracleError $ withTransaction $ do
        void $ execute "insert into transactions_test values(:1)" (Only @Int 6) -- should fail
        -- transaction that inserts rows with savepoint that is rolled back to
      void $ withTransaction $ do
        void $ execute "insert into transactions_test values(:1)" (Only @Int 8)
        handleOracleError $ withTransaction $ do
          void $ execute "insert into transactions_test values(:1)" (Only @Int 9)
          void $ execute "insert into transactions_test values(:1)" (Only @Int 9) -- should fail
        void $ execute "insert into transactions_test values(:1)" (Only @Int 10)
      results <- query_ @(Only Int) "select * from transactions_test"
      void $ execute_ "drop table transactions_test"
      pure $ do
        results `shouldBe` [Only 1 .. Only 8] <> [Only 10]

  describe "concurrency tests" $ do
    it "should handle bare execution (no transaction block)" $ runSpec pool $ do
      void $ execute_ "create table concurrency_test_no_tx (num_column number(10,0) primary key)"
      let rows = [0 .. 100]
      Async.pooledForConcurrentlyN_ 5 rows $ \i -> do
        void $ execute "insert into concurrency_test_no_tx values (:1)" (Only @Int i)
      results <- query_ "select * from concurrency_test_no_tx"
      void $ execute_ "drop table concurrency_test_no_tx"
      pure $ do
        List.sort (coerce @[Only Int] @[Int] results) `shouldBe` rows

    it "should handle concurrent queries" $ runSpec pool $ do
      void $ execute_ "create table concurrency_test_query (num_column number(10,0) primary key)"
      let rows = [0 .. 100]
      void $ executeMany "insert into concurrency_test_query values (:1)" (coerce @[Int] @[Only Int] rows)
      results <- Async.pooledForConcurrentlyN 5 rows $ \i -> do
        queryOne "select * from concurrency_test_query where num_column = :1" (Only @Int i)
      void $ execute_ "drop table concurrency_test_query"
      pure $ do
        List.sort (coerce @[Only Int] @[Int] results) `shouldBe` rows

    it "should handle multiple concurrent transactions" $ runSpec pool $ do
      void $ execute_ "create table concurrency_test (num_column number(10,0) primary key)"
      let rows = [0, 2 .. 100]
      Async.pooledForConcurrentlyN_ 5 rows $ \i -> do
        withTransaction $ do
          void $ execute "insert into concurrency_test values (:1)" (Only @Int i)
          void $ execute "insert into concurrency_test values (:1)" (Only @Int (i + 1))
      results <- query_ "select * from concurrency_test"
      void $ execute_ "drop table concurrency_test"
      pure $ do
        List.sort (coerce @[Only Int] @[Int] results) `shouldBe` concatMap (\i -> [i, i + 1]) rows

    it "should handle multiple concurrent executions inside of transaction" $ runSpec pool $ do
      void $ execute_ "create table concurrency_test_inner (num_column number(10,0) primary key)"
      let rows = [0, 2 .. 100]
      withTransaction $ do
        Async.pooledForConcurrentlyN_ 5 rows $ \i -> do
          void $ execute "insert into concurrency_test_inner values (:1)" (Only @Int i)
          void $ execute "insert into concurrency_test_inner values (:1)" (Only @Int (i + 1))
      results <- query_ "select * from concurrency_test_inner"
      void $ execute_ "drop table concurrency_test_inner"
      pure $ do
        List.sort (coerce @[Only Int] @[Int] results) `shouldBe` concatMap (\i -> [i, i + 1]) rows

    it "should handle multiple concurrent transactions and savepoints" $ runSpec pool $ do
      void $ execute_ "create table concurrency_test_nested (num_column number(10,0) primary key)"
      let rows = [0, 6 .. 100]
      Async.pooledForConcurrentlyN_ 5 rows $ \i -> do
        withTransaction $ do
          void $ execute "insert into concurrency_test_nested values (:1)" (Only @Int i)
          void $ execute "insert into concurrency_test_nested values (:1)" (Only @Int (i + 1))
          withTransaction $ do
            void $ execute "insert into concurrency_test_nested values (:1)" (Only @Int (i + 2))
            void $ execute "insert into concurrency_test_nested values (:1)" (Only @Int (i + 3))
            withTransaction $ do
              void $ execute "insert into concurrency_test_nested values (:1)" (Only @Int (i + 4))
              void $ execute "insert into concurrency_test_nested values (:1)" (Only @Int (i + 5))
      results <- query_ "select * from concurrency_test_nested"
      void $ execute_ "drop table concurrency_test_nested"
      pure $ do
        List.sort (coerce @[Only Int] @[Int] results) `shouldBe` concatMap (\i -> [i + j | j <- [0 .. 5]]) rows

    it "should handle multiple concurrent transactions and savepoint with rollback" $ runSpec pool $ do
      void $ execute_ "create table concurrency_test_nested_fail (num_column number(10,0) primary key)"
      let rows = [0, 4 .. 100]
      Async.pooledForConcurrentlyN_ 5 rows $ \i -> do
        withTransaction $ do
          void $ execute "insert into concurrency_test_nested_fail values (:1)" (Only @Int i)
          void $ execute "insert into concurrency_test_nested_fail values (:1)" (Only @Int (i + 1))
          handleOracleError . withTransaction $ do
            void $ execute "insert into concurrency_test_nested_fail values (:1)" (Only @Int (i + 2))
            void $ execute "insert into concurrency_test_nested_fail values (:1)" (Only @Int (i + 3))
            void $ execute "insert into concurrency_test_nested_fail values (:1)" (Only @Int (i + 3)) -- fail
      results <- query_ "select * from concurrency_test_nested_fail"
      void $ execute_ "drop table concurrency_test_nested_fail"
      pure $ do
        List.sort (coerce @[Only Int] @[Int] results) `shouldBe` concatMap (\i -> [i, i + 1]) rows

handleOracleError :: MonadOracle m => m () -> m ()
handleOracleError action = do
  rs <- Exc.try @_ @OracleError action
  either (const (pure ())) (const (pure ())) rs
