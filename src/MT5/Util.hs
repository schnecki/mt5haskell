module MT5.Util where

import           Data.Time.Clock       (NominalDiffTime, UTCTime)
import           Data.Time.Clock.POSIX
import           Data.Time.LocalTime
import           System.IO.Unsafe      (unsafePerformIO)


-- | Converts seconds since epoch to UTCTime
secondsToUTCTime :: Integer -> UTCTime
secondsToUTCTime seconds =
  let timeInSeconds = fromRational (toRational seconds)
  in posixSecondsToUTCTime timeInSeconds

-- | Converts milliseconds since epoch to UTCTime
mscToUTCTime :: Integer -> UTCTime
mscToUTCTime = millisecondsToUTCTime

-- | Converts milliseconds since epoch to UTCTime
millisecondsToUTCTime :: Integer -> UTCTime
millisecondsToUTCTime millis =
  let timeInSeconds = fromRational (toRational millis / 1000)
  in posixSecondsToUTCTime timeInSeconds
