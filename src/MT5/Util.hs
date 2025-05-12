module MT5.Util where

import           Data.Time.Clock       (NominalDiffTime, UTCTime)
import           Data.Time.Clock.POSIX
import           Data.Time.LocalTime
import           System.IO.Unsafe      (unsafePerformIO)


-- | Converts milliseconds since epoch to UTCTime
secondsToUTCTime :: Integer -> UTCTime
secondsToUTCTime millis =
  unsafePerformIO $ do
    tz <- getCurrentTimeZone
    let offsetSeconds = fromIntegral (timeZoneMinutes tz * 60) :: NominalDiffTime
        timeInSeconds = fromRational (toRational millis)
        utcTime = posixSecondsToUTCTime (timeInSeconds - offsetSeconds)
    return utcTime

-- | Converts milliseconds since epoch to UTCTime
mscToUTCTime :: Integer -> UTCTime
mscToUTCTime = millisecondsToUTCTime

-- | Converts milliseconds since epoch to UTCTime
millisecondsToUTCTime :: Integer -> UTCTime
millisecondsToUTCTime millis =
  unsafePerformIO $ do
    tz <- getCurrentTimeZone
    let offsetSeconds = fromIntegral (timeZoneMinutes tz * 60) :: NominalDiffTime
        timeInSeconds = fromRational (toRational millis / 1000)
        utcTime = posixSecondsToUTCTime (timeInSeconds - offsetSeconds)
    return utcTime
