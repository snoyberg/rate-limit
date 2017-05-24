#!/usr/bin/env stack
-- stack --resolver lts-8.12 script
import Test.Hspec
import RateLimit
import System.Clock
import Control.Concurrent.Async
import Control.Monad

main :: IO ()
main = hspec $ do
  it "actually delays with too many tickles" $ do
    rl <- newRateLimit 1 500000
    startTime <- getTime Monotonic
    tickle rl
    tickle rl
    tickle rl
    endTime <- getTime Monotonic
    let diff = endTime `diffTimeSpec` startTime
    sec diff `shouldSatisfy` (>= 1)
  it "actually delays with too many tickles in parallel" $ do
    rl <- newRateLimit 1 500000
    startTime <- getTime Monotonic
    replicateConcurrently_ 3 $ tickle rl
    endTime <- getTime Monotonic
    let diff = endTime `diffTimeSpec` startTime
    sec diff `shouldSatisfy` (>= 1)
  it "no delays with higher threshold" $ do
    rl <- newRateLimit 100 500000000
    startTime <- getTime Monotonic
    replicateM_ 90 $ tickle rl
    endTime <- getTime Monotonic
    let diff = endTime `diffTimeSpec` startTime
    sec diff `shouldSatisfy` (< 1)
