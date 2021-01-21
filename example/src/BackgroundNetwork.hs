
module BackgroundNetwork where


import Prod.Background (background, BackgroundVal, readBackgroundVal)
import Prod.Tracer (Tracer(..), tracePrint, pulls, silent)
import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan)
import Control.Concurrent (threadDelay)
import Control.Monad (forever)


-- | Showcases how one could use channels to perform scheduling
-- between different background values (some are polled, some are pushed, one is combining)
complicatedNetworkOfBackgroundUpdates :: Tracer IO Int -> IO (BackgroundVal Int)
complicatedNetworkOfBackgroundUpdates traceInt = do

  -- a first background task continuously write to a channel
  c <- newChan
  _ <- background silent nostate 0 (act0 c)

  -- a second background task actually (which happens to do nothing) has a value
  b1 <- background silent nostate 500 act1

  -- the composed background tasks actually is composed of a combination of
  -- - the updates sent on the channel by the first background job
  -- - and the second background job (read on demand)
  background silent 0 0 (act2 (readBackgroundVal b1) c)
  where
    nostate = ()

    stateless :: IO a -> (() -> IO (a, ()))
    stateless io = const $ (,) <$> io <*> pure ()

    act0 :: Chan Int -> () -> IO (Int, ())
    act0 c = stateless (threadDelay 10000000 >> writeChan c 1 >> pure 1)

    act1 :: () -> IO (Int, ())
    act1 = stateless (forever $ threadDelay 10000000)

    act2 :: IO Int -> Chan Int -> Int -> IO (Int, Int)
    act2 io c n = do
        m <- readChan c
        let ret = n + m
        runTracer (pulls (\k -> (+k) <$> io) traceInt) ret
        pure $ (ret, ret)
