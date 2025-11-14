{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.Oracle.Simple.Internal.Timestamp
  ( DPITimestamp (..),
    dpiTimeStampToUTCDPITimeStamp,
    utcTimeToDPITimestamp,
  )
where

import Data.Int (Int16, Int8)
import Data.Time
  ( LocalTime (..),
    TimeOfDay (..),
    TimeZone (..),
    UTCTime (..),
    ZonedTime (..),
    toGregorian,
    utc,
    utcToZonedTime,
  )
import qualified Data.Time as Time
import Data.Word (Word32, Word8)
import Foreign.C.Types (CInt (..))
import Foreign.Storable.Generic (GStorable)
import GHC.Generics (Generic)

data DPIIntervalDS = DPIIntervalDS
  { days :: CInt
  , hours :: CInt
  , minutes :: CInt
  , seconds :: CInt
  , fseconds :: CInt
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (GStorable)

data DPIIntervalYM = DPIIntervalYM
  { years :: CInt
  , months :: CInt
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (GStorable)

data DPITimestamp = DPITimestamp
  { year :: Int16
  , month :: Word8
  , day :: Word8
  , hour :: Word8
  , minute :: Word8
  , second :: Word8
  , fsecond :: Word32
  , tzHourOffset :: Int8
  , tzMinuteOffset :: Int8
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (GStorable)

{- | Converts a DPITimestamp into the UTCTime zone by applying the offsets
to the year, month, day, hour, minutes and seconds
-}
dpiTimeStampToUTCDPITimeStamp :: DPITimestamp -> DPITimestamp
dpiTimeStampToUTCDPITimeStamp dpi@DPITimestamp {..} =
  let offsetInMinutes, currentMinutes :: Int
      offsetInMinutes = negate $ (fromIntegral tzHourOffset * 60) + fromIntegral tzMinuteOffset
      currentMinutes = (fromIntegral hour * 60) + fromIntegral minute

      (hours, minutes) = ((currentMinutes + offsetInMinutes) `mod` 1440) `quotRem` 60
      gregorianDay = Time.fromGregorian (fromIntegral year) (fromIntegral month) (fromIntegral day)
      updatedDay
        | currentMinutes + offsetInMinutes > 1440 = Time.addDays 1 gregorianDay
        | currentMinutes + offsetInMinutes < 0 = Time.addDays (-1) gregorianDay
        | otherwise = gregorianDay
      (year', month', day') = Time.toGregorian updatedDay
   in dpi
        { tzHourOffset = 0
        , tzMinuteOffset = 0
        , year = fromIntegral year'
        , month = fromIntegral month'
        , day = fromIntegral day'
        , hour = fromIntegral hours
        , minute = fromIntegral minutes
        }

utcTimeToDPITimestamp :: UTCTime -> DPITimestamp
utcTimeToDPITimestamp utcTime = dpiTimeStampToUTCDPITimeStamp dpiTs
  where
    ZonedTime {..} = utcToZonedTime utc utcTime
    LocalTime {..} = zonedTimeToLocalTime
    (year, month, day) = toGregorian localDay
    TimeOfDay {..} = localTimeOfDay
    TimeZone {..} = zonedTimeZone
    (seconds, fractionalSeconds) = properFraction todSec
    (hourOffset, minuteOffset) = timeZoneMinutes `quotRem` 60
    dpiTs =
      DPITimestamp
        { year = fromIntegral year
        , month = fromIntegral month
        , day = fromIntegral day
        , hour = fromIntegral todHour
        , minute = fromIntegral todMin
        , second = seconds
        , fsecond = truncate (fractionalSeconds * 1e9)
        , tzHourOffset = fromIntegral hourOffset
        , tzMinuteOffset = fromIntegral minuteOffset
        }
