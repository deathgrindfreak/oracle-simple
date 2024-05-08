{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module WaitForOracle
  ( waitForOracle,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception.Safe (MonadCatch, MonadThrow)
import qualified Control.Exception.Safe as Exc
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT (..), ask, local)
import qualified Data.Maybe as Maybe
import qualified System.Clock as Clock
import qualified System.Exit as Exit
import qualified System.Timeout as Timeout
import UnliftIO (MonadUnliftIO)

import Database.Oracle.Simple
  ( ConnectionParams,
    HasOracleContext (..),
    MonadOracle,
    OracleEnv,
    OracleError,
    newOracleEnv,
    ping,
    withPool,
  )

newtype WaitM a = WaitM {runWaitM :: ReaderT OracleEnv IO a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadOracle
    , MonadThrow
    , MonadCatch
    , MonadFail
    , MonadUnliftIO
    )

instance HasOracleContext WaitM where
  getOracleEnv = WaitM ask
  localOracleEnv f = WaitM . local f . runWaitM

runWait :: ConnectionParams -> WaitM a -> IO a
runWait dbParams waitM =
  withPool dbParams $ runReaderT (runWaitM waitM) . newOracleEnv

waitForOracle :: ConnectionParams -> Int -> Int -> IO ()
waitForOracle dbParams timeoutSeconds sleepSeconds = do
  putStrLn $ "wait-for-oracle: Waiting " <> show timeoutSeconds <> "s for Oracle DB to start ..."

  didTimeout <- Timeout.timeout (toMicroSeconds timeoutSeconds) $ do
    start <- Clock.getTime Clock.Monotonic
    let
      wait = do
        p <- Exc.try @_ @OracleError $ runWait dbParams ping
        case p of
          Right pingSuccessful ->
            if pingSuccessful
              then do
                end <- Clock.getTime Clock.Monotonic
                let
                  secondsWaited = Clock.toNanoSecs (Clock.diffTimeSpec end start) `div` 1_000_000_000
                putStrLn $ "wait-for-oracle: Oracle DB available after " <> show secondsWaited <> " seconds"
              else do
                threadDelay (toMicroSeconds sleepSeconds)
                wait
          Left _ -> do
            threadDelay (toMicroSeconds sleepSeconds)
            wait
    wait

  unless (Maybe.isJust didTimeout) $ do
    putStrLn $ "wait-for-oracle: Timed out waiting for Oracle DB to start"
    Exit.exitFailure

toMicroSeconds :: Int -> Int
toMicroSeconds sec = sec * 1_000 * 1_000
