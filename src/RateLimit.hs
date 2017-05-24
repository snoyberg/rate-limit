{-# LANGUAGE RecordWildCards #-}
module RateLimit
  ( RateLimit
  , newRateLimit
  , tickle
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception.Safe (mask)
import Control.Monad
import System.Clock

-- | Opaque type representing a rate limit
--
-- @since 0.1.0.0
data RateLimit = RateLimit
  { currCount :: !(TVar Int)
  , allowedRequests :: !Int
  , resetDelay :: !Int
  }

-- | Create a new 'RateLimit' value
--
-- @since 0.1.0.0
newRateLimit :: Int -- ^ number of requests
             -> Int -- ^ how often to reset to 0, in microseconds
             -> IO RateLimit
newRateLimit allowedRequests resetDelay = do
  currCount <- newTVarIO 0
  return RateLimit {..}

tickle :: RateLimit -> IO ()
tickle RateLimit {..} = mask $ \restore -> do
  maybeLaunchThread <- restore $ atomically $ do
    currCount' <- readTVar currCount
    check $ currCount' < allowedRequests
    writeTVar currCount $! currCount' + 1
    return $
      when (currCount' == 0) $ void $ async $ do
          threadDelay resetDelay
          atomically $ writeTVar currCount 0
  maybeLaunchThread
