{-# LANGUAGE ScopedTypeVariables #-}
module MT5.Util where

import           Data.Time.Clock       (UTCTime)
import           Data.Time.Clock.POSIX


-- | Converts seconds since epoch to UTCTime, treating the epoch as already
--   being in UTC.  MT5's @copy_rates_*@ / @symbol_info_tick@ report timestamps
--   in the /broker trade-server timezone/ (typically EET, UTC+2 / UTC+3 with
--   DST) rather than UTC, so this raw conversion is only correct for genuine
--   UTC epochs (e.g. literal @0@ sentinels).  For server-sourced timestamps use
--   'serverSecondsToUTCTime', which subtracts the cached server offset.
secondsToUTCTime :: Integer -> UTCTime
secondsToUTCTime seconds =
  let timeInSeconds = fromRational (toRational seconds)
  in posixSecondsToUTCTime timeInSeconds

-- | Converts milliseconds since epoch to UTCTime (raw, UTC-assumed).  See
--   'secondsToUTCTime' for the server-timezone caveat and 'serverMscToUTCTime'
--   for the offset-corrected variant.
mscToUTCTime :: Integer -> UTCTime
mscToUTCTime = millisecondsToUTCTime

-- | Converts milliseconds since epoch to UTCTime
millisecondsToUTCTime :: Integer -> UTCTime
millisecondsToUTCTime millis =
  let timeInSeconds = fromRational (toRational millis / 1000)
  in posixSecondsToUTCTime timeInSeconds

---------------------------------------------------------------------------------------------------
-- Timestamp conversion (UTC-native)
---------------------------------------------------------------------------------------------------
--
-- MT5's @copy_rates_range@ / @copy_rates_from@ accept a /timezone-aware/ UTC
-- 'datetime' (the bridge tags every request instant with @tzinfo=UTC@) and, for
-- such input, return bar open times already in true UTC — no broker-server
-- offset is involved on either side.  The functions below are therefore straight
-- UTC conversions; the historical broker-timezone offset machinery is gone.
--
-- The un-anchored calls @copy_rates_from_pos@ and @symbol_info_tick@ /do/ emit
-- server-local wall-clock, but the request layer no longer uses them: recent
-- pulls are expressed as a now-anchored 'copy_rates_range', and the live price
-- poll timestamps its tick with the client UTC clock rather than @tick.time@.

-- | Convert an MT5 /seconds/ epoch (already true UTC from a tz-aware
--   @copy_rates_*@ call) to 'UTCTime'.
serverSecondsToUTCTime :: Integer -> UTCTime
serverSecondsToUTCTime = secondsToUTCTime

-- | Convert an MT5 /milliseconds/ epoch (already true UTC) to 'UTCTime'.
serverMscToUTCTime :: Integer -> UTCTime
serverMscToUTCTime = mscToUTCTime

-- | Request instants are already true UTC and the bridge tags them
--   @tzinfo=UTC@, so no shift is applied before formatting.
utcToServerTime :: UTCTime -> UTCTime
utcToServerTime = id
