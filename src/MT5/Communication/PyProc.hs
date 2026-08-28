{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MT5.Communication.PyProc
    ( PyProc (..)
    , pyProc
    , mt5DaemonOwner
    , withMT5Lock
    , registerReconnectAction
    , MT5TimeoutException (..)
    , mt5CycleTimeoutMicros
    ) where

import           Control.Concurrent         (threadDelay)
import           Control.Concurrent.MVar    (MVar, modifyMVar, newMVar,
                                             withMVar)
import           Control.Exception          (Exception, IOException, catch,
                                             finally, throwIO, try)
import           Data.IORef
import           Data.Maybe                 (fromMaybe)
import           GHC.Clock                  (getMonotonicTimeNSec)
import           System.Environment         (lookupEnv)
import           System.IO
import           System.IO.Error            (isEOFError,
                                             isResourceVanishedError)
import           System.IO.Unsafe           (unsafePerformIO)
import           Text.Read                  (readMaybe)
import           System.Posix.IO            (handleToFd)
import           System.Posix.IO.ByteString (LockRequest (..), setLock,
                                             waitToSetLock)
import           System.Posix.Types         (Fd, FileOffset)
import           System.Timeout             (timeout)

import           MT5.Communication.Socket   (socketPath)


-- | Connection to the MT5 daemon (socket-based).
--
-- Both 'pyIn' and 'pyOut' point to the same socket handle; kept separate so
-- the 'send' / 'receive' call-sites need no changes.
data PyProc = PyProc
    { pyIn      :: !Handle  -- ^ Read handle (socket)
    , pyOut     :: !Handle  -- ^ Write handle (same socket)
    , pyCleanup :: !(IO ()) -- ^ Close the connection
    }

-- | Active daemon connection; Nothing until 'startMT5' has been called.
pyProc :: IORef (Maybe PyProc)
pyProc = unsafePerformIO $ newIORef Nothing
{-# NOINLINE pyProc #-}

-- | True when this process started the daemon (and is responsible for
-- sending SHUTDOWN and stopping the RPyC server thread).
mt5DaemonOwner :: IORef Bool
mt5DaemonOwner = unsafePerformIO $ newIORef False
{-# NOINLINE mt5DaemonOwner #-}

-- | Global mutex serialising all send/receive exchanges with the MT5 daemon.
-- The Python bridge uses a single shared socket handle; concurrent threads
-- interleave their send/receive sequences and read each other's responses.
-- Every complete request/response cycle must be wrapped with this lock.
pyProcLock :: MVar ()
pyProcLock = unsafePerformIO $ newMVar ()
{-# NOINLINE pyProcLock #-}

-- | Reconnect callback registered by 'MT5.Init.connectToDaemon'.
-- 'withMT5Lock' invokes this when a broken-pipe or EOF error is detected,
-- then retries the action once.  Starts as a no-op until Init registers it.
pyProcReconnectAction :: IORef (IO ())
pyProcReconnectAction = unsafePerformIO $ newIORef (return ())
{-# NOINLINE pyProcReconnectAction #-}

-- | Register the reconnect action (called by 'MT5.Init' after each successful connect).
registerReconnectAction :: IO () -> IO ()
registerReconnectAction = writeIORef pyProcReconnectAction

-- | Thrown when a full MT5 request/response cycle exceeds
-- 'mt5CycleTimeoutMicros' twice (initial attempt plus one reconnect retry).
-- Distinct from EOF/broken-pipe: the socket stays open but the daemon never
-- replies (terminal busy, broker connection dropped, Python GIL stall).
-- Carries the elapsed budget in microseconds.
newtype MT5TimeoutException = MT5TimeoutException Int
  deriving (Show)

instance Exception MT5TimeoutException

-- | Per-cycle deadline for a full MT5 request/response exchange.
--
-- The daemon transport does a blocking socket read for the reply under
-- 'pyProcLock'.  Without a deadline, a daemon that stalls /without/ closing
-- the socket blocks that read forever, never releases the lock, and wedges
-- every subsequent MT5 call across all instruments.  Bounding the cycle turns
-- a permanent freeze into a recoverable, retryable error.
--
-- Overridable per process via the @MT5_CYCLE_TIMEOUT_MICROS@ environment
-- variable (integer microseconds).  A bulk data-collection process shares the
-- daemon with a latency-sensitive live trader: because a stalled request holds
-- the cross-process work lock for its whole deadline, the collector sets a much
-- shorter budget so a hanging historical fetch releases the shared daemon
-- quickly instead of blocking the live trader for the full default window.  An
-- invalid or absent value falls back to the 30s default.
mt5CycleTimeoutMicros :: Int
mt5CycleTimeoutMicros = unsafePerformIO $ do
  mEnv <- lookupEnv "MT5_CYCLE_TIMEOUT_MICROS"
  return $ fromMaybe defaultMicros (mEnv >>= readMaybe >>= clampPositive)
  where
    defaultMicros = 30 * 1000 * 1000  -- 30s
    clampPositive n
      | n > 0     = Just n
      | otherwise = Nothing
{-# NOINLINE mt5CycleTimeoutMicros #-}

-- | Run an IO action as an atomic MT5 request/response cycle.
--
-- The whole cycle is bounded by 'mt5CycleTimeoutMicros'.  A timeout, a
-- broken pipe, or an EOF are all treated as recoverable: the registered
-- reconnect action runs and the cycle is retried exactly once.  A second
-- failure throws — a timeout as 'MT5TimeoutException', otherwise the original
-- 'IOException' — so upstream retry/backoff handles it and the engine moves
-- on to the next instrument instead of hanging.
-- | Filesystem path of the advisory lock guarding daemon access across
-- separate OS processes.  Keyed on the shared daemon socket so every process
-- talking to the same MT5 terminal contends on the same lock.
crossProcLockPath :: FilePath
crossProcLockPath = socketPath ++ ".lock"

-- | Persistent file descriptor holding the cross-process advisory lock.
--
-- 'Nothing' until first use; a 'Just' that stays 'Nothing' after a failed
-- open means the lock is disabled (degrade to intra-process 'pyProcLock'
-- only) rather than crashing a process that cannot create the lock file.
mt5CrossProcFd :: MVar (Maybe Fd)
mt5CrossProcFd = unsafePerformIO $ newMVar Nothing
{-# NOINLINE mt5CrossProcFd #-}

-- | POSIX record lock on the /work/ byte (offset 0, length 1).
--
-- This is the real serialising lock: held for an entire request/response
-- cycle.  Acquired by non-blocking polling ('setLock'/@F_SETLK@) so the wait is
-- interruptible by 'timeout' — a wedged daemon holding it must not pin an
-- uninterruptible @safe@ FFI call.
workRegion :: LockRequest -> (LockRequest, SeekMode, FileOffset, FileOffset)
workRegion req = (req, AbsoluteSeek, 0, 1)

-- | POSIX record lock on the /turnstile/ byte (offset 1, length 1).
--
-- A fairness gate, disjoint from 'workRegion'.  Acquired by a /blocking/
-- 'waitToSetLock' (@F_SETLKW@) so the kernel queues contending processes in
-- arrival order, then released the instant the work lock is held.  A process
-- that has just released the work lock and loops back must re-enter at the tail
-- of this queue, so a hot request loop (e.g. a historical backfill) can no
-- longer re-grab the work lock ahead of a process already waiting — which is
-- what let a data download starve the live trader off the shared daemon.
turnstileRegion :: LockRequest -> (LockRequest, SeekMode, FileOffset, FileOffset)
turnstileRegion req = (req, AbsoluteSeek, 1, 1)

-- | Lazily open (once) the descriptor backing 'crossProcLockPath'.
--
-- A failure to open disables cross-process locking for this process instead
-- of propagating: intra-process serialisation via 'pyProcLock' still holds.
getCrossProcFd :: IO (Maybe Fd)
getCrossProcFd = modifyMVar mt5CrossProcFd $ \case
  Just fd -> return (Just fd, Just fd)
  Nothing -> do
    r <- try (openFile crossProcLockPath ReadWriteMode >>= handleToFd)
    case r of
      Right fd               -> return (Just fd, Just fd)
      Left (_ :: IOException) -> return (Nothing, Nothing)

-- | Serialise the enclosed cycle across processes sharing the MT5 daemon,
-- fairly.
--
-- A two-lock turnstile mutex.  A process first blocks on the 'turnstileRegion'
-- (@F_SETLKW@, kernel-queued in arrival order), then polls the 'workRegion'
-- (@F_SETLK@, interruptible), then releases the turnstile as soon as the work
-- lock is held.  Because a process that has just released the work lock must
-- re-join the tail of the turnstile queue, a tight request loop can no longer
-- re-acquire ahead of an already-waiting peer: this is what stops a historical
-- data backfill from starving the live trader off the shared daemon.
--
-- The work lock is polled (not blocking) so a wedged daemon holding it cannot
-- pin an uninterruptible @safe@ FFI call under 'timeout'; if it cannot be
-- acquired within 'acquireBudgetMicros' the cycle fails as an
-- 'MT5TimeoutException', matching the daemon-timeout path so upstream
-- retry/backoff handles it.  When the lock file is unavailable the action runs
-- unguarded (intra-process 'pyProcLock' still applies).
withCrossProcLock :: forall a. IO a -> IO a
withCrossProcLock act = getCrossProcFd >>= \case
  Nothing -> act
  Just fd -> do
    -- Block in the fair turnstile queue, then take the work lock; release the
    -- turnstile the moment work is held (or acquisition fails), so waiters are
    -- served strictly in arrival order.
    acquired <-
      (waitToSetLock fd (turnstileRegion WriteLock) `catch` \(_ :: IOException) -> return ())
        >> pollAcquire fd 0 `finally` releaseTurnstile fd
    if acquired
      then act `finally` releaseWork fd
      else throwIO (MT5TimeoutException mt5CycleTimeoutMicros)
  where
    stepMicros :: Int
    stepMicros = 50 * 1000  -- 50ms poll interval

    -- Waiting for a peer process to release the lock is legitimate contention,
    -- not a daemon stall, so it gets its own generous budget rather than the
    -- per-cycle daemon deadline. A crashed holder auto-releases the fcntl lock
    -- on process exit, and a live-but-wedged holder is itself bounded by its
    -- own 'mt5CycleTimeoutMicros', so this budget is only ever approached under
    -- pathological contention.
    acquireBudgetMicros :: Int
    acquireBudgetMicros = 60 * 1000 * 1000  -- 1 min

    -- Non-blocking acquire loop for the work lock, bounded by the contention
    -- budget.  Runs while the turnstile is held, so at most the single prior
    -- work holder is contended against.
    pollAcquire :: Fd -> Int -> IO Bool
    pollAcquire fd waited
      | waited >= acquireBudgetMicros = return False
      | otherwise = do
          got <- (setLock fd (workRegion WriteLock) >> return True)
                   `catch` \(_ :: IOException) -> return False
          if got
            then return True
            else threadDelay stepMicros >> pollAcquire fd (waited + stepMicros)

    -- Release helpers; a failed unlock must never mask the cycle result.
    releaseWork :: Fd -> IO ()
    releaseWork fd =
      setLock fd (workRegion Unlock)
        `catch` \(_ :: IOException) -> return ()

    releaseTurnstile :: Fd -> IO ()
    releaseTurnstile fd =
      setLock fd (turnstileRegion Unlock)
        `catch` \(_ :: IOException) -> return ()

withMT5Lock :: forall a. IO a -> IO a
withMT5Lock action = withMVar pyProcLock $ \_ -> withCrossProcLock $ do
  firstAttempt <- attempt
  case firstAttempt of
    Right a -> return a
    Left _  -> do
      reconnect <- readIORef pyProcReconnectAction
      reconnect
      retryAttempt <- attempt
      case retryAttempt of
        Right a -> return a
        Left _  -> throwIO (MT5TimeoutException mt5CycleTimeoutMicros)
  where
    -- One bounded cycle.  'Left ()' signals a recoverable failure (deadline
    -- exceeded, or a vanished/EOF socket); anything else is rethrown.
    attempt :: IO (Either () a)
    attempt = timedRun `catch` \e ->
      if isResourceVanishedError e || isEOFError (e :: IOException)
        then return (Left ())
        else throwIO e

    -- Deadline detection must not rely solely on 'timeout' returning
    -- 'Nothing': call sites wrap the cycle in @try :: IO (Either
    -- SomeException a)@, which catches 'timeout'\'s internal async exception
    -- and turns a timeout into @Just (Left _)@.  The async exception still
    -- unblocks the stalled socket read, so we additionally classify any cycle
    -- whose wall-clock elapsed reached the budget as a timeout — forcing a
    -- reconnect that drops the possibly-desynced socket before retrying.
    timedRun :: IO (Either () a)
    timedRun = do
      t0 <- getMonotonicTimeNSec
      m  <- timeout mt5CycleTimeoutMicros action
      t1 <- getMonotonicTimeNSec
      let elapsedMicros = fromIntegral ((t1 - t0) `div` 1000) :: Int
      return $ case m of
        Nothing -> Left ()
        Just a
          | elapsedMicros >= mt5CycleTimeoutMicros -> Left ()
          | otherwise                              -> Right a
