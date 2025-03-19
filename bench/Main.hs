{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Control.DeepSeq (NFData, force)
import Control.Monad (void)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Time as Time
import GHC.Generics (Generic)
import Test.Tasty.Bench
import qualified UnliftIO as UIO
import UnliftIO.Exception (tryAny)

import Database.Oracle.Simple

data InsertTest = MkInsertTest
  { int :: Int
  , str :: String
  , dub :: Double
  , when :: Maybe Time.UTCTime
  , txt :: T.Text
  }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (NFData, ToRow, ToBinding)

numSamples :: Int
numSamples = 100

samples :: [InsertTest]
samples =
  cycle
    [ MkInsertTest 100 "hello" 1.2 (Just (Time.UTCTime (Time.fromGregorian 2024 1 1) 0)) "ah"
    , MkInsertTest 200 "world" 2.3 Nothing "ahh"
    , MkInsertTest 42 "thisisalongerstring" 9999912312412412412.9992414124299 Nothing "ahhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh"
    ]

neSamples :: NE.NonEmpty InsertTest
neSamples = NE.fromList samples

main :: IO ()
main = do
  ps <- params
  withPool ps $ \(newOracleEnv -> oenv) -> do
    void . runOracleT oenv $ tryAny $ execute_ "drop table test"
    void
      . runOracleT oenv
      $ execute_ "create table test(num number(10,0), str varchar2(50 char), double binary_double, somewhen timestamp, txt varchar2(50 char))"
    defaultMain
      [ env (UIO.evaluate (force (take numSamples samples))) $ \list ->
          env (UIO.evaluate (force (NE.fromList $ NE.take numSamples neSamples))) $ \neList ->
            bgroup
              "Insert Rows"
              [ bench "executeMany" $
                  nfAppIO
                    (runOracleT oenv . executeMany "insert into test values (:1,:2,:3,:4,:5)")
                    list
              , bench "executeManyArray" $
                  nfAppIO
                    (runOracleT oenv . executeManyArray "insert into test values (:1,:2,:3,:4,:5)")
                    neList
              ]
      ]

params :: IO ConnectionParams
params = do
  defPoolParams <- defaultPoolCreateParams
  defCommonCreateParams <- defaultCommonCreateParams
  pure $
    ConnectionParams
      "username"
      "password"
      "dev-db:1521/devdb"
      ( Just $
          defCommonCreateParams
            { encoding = "UTF8"
            }
      )
      ( Just $
          defPoolParams
            { minSessions = 20
            , sessionIncrement = 10
            , maxSessions = 1000
            , timeout = 5 * 60
            , waitTimeout = 5 * 60 * 1000
            , maxLifetimeSession = 5 * 60
            }
      )
