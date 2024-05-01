{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module WaitForOracle
  ( waitForOracle,
  )
where

import Database.Oracle.Simple (ConnectionParams, OracleError, ping, withPool, withPoolConnection)

import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import qualified Control.Monad.Catch as Catch
import qualified Control.Monad.IO.Class as MIO
import qualified Data.Maybe as Maybe
import qualified System.Clock as Clock
import qualified System.Exit as Exit
import qualified System.Timeout as Timeout

waitForOracle :: ConnectionParams -> Int -> Int -> IO ()
waitForOracle dbParams timeoutSeconds sleepSeconds = do
  putStrLn $ "wait-for-oracle: Waiting " <> show timeoutSeconds <> "s for Oracle DB to start ..."

  didTimeout <- Timeout.timeout (toMicroSeconds timeoutSeconds) $ do
    start <- MIO.liftIO $ Clock.getTime Clock.Monotonic
    let
      wait = do
        p <-
          Catch.try @_ @OracleError $
            withPool dbParams $
              \pool -> withPoolConnection pool ping

        case p of
          Right pingSuccessful ->
            if pingSuccessful
              then do
                end <- MIO.liftIO $ Clock.getTime Clock.Monotonic
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
