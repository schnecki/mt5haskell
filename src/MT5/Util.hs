{-# LANGUAGE ScopedTypeVariables #-}
module MT5.Util where

import           Data.IORef            (IORef, newIORef, readIORef, writeIORef)
import           Data.Time.Clock       (NominalDiffTime, UTCTime (..), addUTCTime)
import           Data.Time.Clock.POSIX
import           Data.Time.LocalTime   (timeZoneMinutes)
import           Data.Time.Zones       (TZ, timeZoneForUTCTime)
import           System.IO.Unsafe      (unsafePerformIO)


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
-- Timestamp conversion (broker-server timezone <-> UTC)
---------------------------------------------------------------------------------------------------
--
-- Empirically an MT5 terminal reports and interprets @copy_rates_*@ timestamps
-- in its /trade-server local wall-clock/ even when the bridge tags request
-- instants @tzinfo=UTC@ — the returned bar times are the server wall-clock, and
-- a request instant is matched against the server clock, not UTC.  Both
-- directions must therefore apply the broker offset explicitly:
--
--   * ingest ('serverSecondsToUTCTime' \/ 'serverMscToUTCTime'): SUBTRACT the
--     offset to turn a server-local timestamp into true UTC;
--   * request ('utcToServerTime'): ADD the offset so a true-UTC window is sent
--     as the server-local string the terminal expects.
--
-- The offset is taken /per instant/ from the broker trade-server's configured
-- IANA zone (see 'MT5.Config.serverTimeZone'), so DST transitions, sub-hour
-- offsets and the full history are all handled exactly for any broker.  The zone
-- is installed once at startup by 'MT5.API.initialize' via 'setServerTZ'; the
-- pure converters read it through a global 'IORef' because they run deep inside
-- candle parsing.  A missing zone is a fatal misconfiguration and 'error's here
-- (unreachable in practice: every call site is behind a live MT5 connection,
-- which 'initialize' refuses to establish without a zone).  As a cross-check,
-- 'offsetFromServerEpoch' turns a live tick's server epoch into the observed
-- offset so callers can warn if it disagrees with the configured zone.

-- | The configured broker trade-server IANA zone, installed once at startup.
--   A global 'IORef' because the timestamp converters are pure yet must observe
--   it.  'Nothing' until 'setServerTZ' runs; reading it unset is fatal.
{-# NOINLINE serverTZRef #-}
serverTZRef :: IORef (Maybe TZ)
serverTZRef = unsafePerformIO (newIORef Nothing)

-- | Install the broker trade-server zone (call once, at 'MT5.API.initialize').
setServerTZ :: TZ -> IO ()
setServerTZ = writeIORef serverTZRef . Just

-- | The installed zone, if any.
lookupServerTZ :: IO (Maybe TZ)
lookupServerTZ = readIORef serverTZRef

-- | Observed broker-server↔UTC offset from a raw @symbol_info_tick@ server epoch
--   (seconds) and the client UTC clock at sampling.  'Nothing' for an implausible
--   difference (@|Δ| > 14h@, e.g. a @0@ epoch when the market has no ticks) so a
--   caller can skip a bogus cross-check rather than warn spuriously.  Used only
--   to validate the configured zone, never to drive conversion.
offsetFromServerEpoch :: Integer -> UTCTime -> Maybe NominalDiffTime
offsetFromServerEpoch serverEpoch nowUtc =
  let nowPosix = realToFrac (utcTimeToPOSIXSeconds nowUtc) :: Double
      diffSec  = fromIntegral serverEpoch - nowPosix
  in if abs diffSec > 14 * 3600 then Nothing else Just (fromRational (toRational diffSec))

-- | Broker-server↔UTC offset (seconds east of UTC) for the instant @u@, read
--   per-instant from the configured trade-server zone.  'error's if no zone was
--   installed — a fatal misconfiguration that 'MT5.API.initialize' prevents.
{-# NOINLINE serverOffsetSeconds #-}
serverOffsetSeconds :: UTCTime -> NominalDiffTime
serverOffsetSeconds u = unsafePerformIO $ do
  mtz <- readIORef serverTZRef
  case mtz of
    Nothing -> error "MT5.Util.serverOffsetSeconds: broker trade-server time zone not configured (set MT5.Config.serverTimeZone via withServerTimeZone). Fatal: candle timestamps cannot be converted to UTC."
    Just tz -> pure $ fromIntegral (timeZoneMinutes (timeZoneForUTCTime tz u) * 60)

-- | Convert an MT5 /seconds/ epoch reported in broker-server local wall-clock to
--   true 'UTCTime'.  Two passes so the (possibly DST-varying) offset is resolved
--   at the corrected instant rather than the raw server label.
serverSecondsToUTCTime :: Integer -> UTCTime
serverSecondsToUTCTime = serverLocalToUTC . secondsToUTCTime

-- | As 'serverSecondsToUTCTime' for a /milliseconds/ epoch.
serverMscToUTCTime :: Integer -> UTCTime
serverMscToUTCTime = serverLocalToUTC . mscToUTCTime

-- | Shift a broker-server-local instant (mislabelled as UTC) back to true UTC by
--   subtracting the zone offset resolved at the corrected instant.
serverLocalToUTC :: UTCTime -> UTCTime
serverLocalToUTC lbl =
  let off1 = serverOffsetSeconds lbl
      off2 = serverOffsetSeconds (addUTCTime (negate off1) lbl)
  in addUTCTime (negate off2) lbl

-- | Convert a true-UTC request instant to the broker-server local wall-clock the
--   terminal matches against, by ADDING the zone offset at that instant.
utcToServerTime :: UTCTime -> UTCTime
utcToServerTime u = addUTCTime (serverOffsetSeconds u) u
