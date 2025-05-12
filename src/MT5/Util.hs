module MT5.Util where

import           Data.Time.Clock       (NominalDiffTime, UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)

secondsToUTCTime :: Integer -> UTCTime
secondsToUTCTime = posixSecondsToUTCTime . fromIntegral

mscToUTCTime :: Integer -> UTCTime
mscToUTCTime = millisecondsToUTCTime . fromIntegral

-- | Converts milliseconds since epoch to UTCTime
millisecondsToUTCTime :: Integer -> UTCTime
millisecondsToUTCTime millis = posixSecondsToUTCTime seconds
  where
    seconds :: NominalDiffTime
    seconds = fromRational (toRational millis / 1000)
