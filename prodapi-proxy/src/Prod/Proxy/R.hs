module Prod.Proxy.R where

import Control.Applicative
import Control.Monad.IO.Class
import qualified Network.Wai as Wai
import System.Random.Shuffle (shuffleM)

import qualified Prod.Healthcheck as Healthcheck
import Prod.Proxy.Base

-- | A monad to help building LookupHostPort functions from composable bricks.
newtype R a = R { run :: Wai.Request -> IO (Maybe a) }

toLookup :: R (Host,Port) -> LookupHostPort
toLookup = run

instance Functor R where
  fmap f (R pa) = R ((fmap . fmap . fmap) f pa)

instance Applicative R where
  pure x = R ((pure . pure . pure) x)
  pf <*> px = R $ \req -> do
    mf1 <- run pf $ req
    case mf1 of
      Nothing -> pure Nothing
      Just f1 -> do
        mx  <- run px $ req
        case mx of
          Nothing -> pure Nothing
          Just x -> pure $ Just $ f1 x

instance Alternative R where
  empty = R (const $ pure Nothing)
  r1 <|> r2 = R $ \req -> do
    x1 <- run r1 req
    case x1 of
      Just _ -> pure x1
      Nothing -> run r2 req

instance Monad R where
  pa >>= pf = R $ \req -> do
    ma <- run pa $ req
    case ma of
      Nothing -> pure Nothing
      Just a -> do
        run (pf a) req

instance MonadIO R where
  liftIO = io

-- Special cases

request :: (Wai.Request -> IO a) -> R a
request f = R $ \req -> fmap Just (f req)

request1 :: (Wai.Request -> IO (Maybe a)) -> R a
request1 = R

io :: IO a -> R a
io x = R $ \_ -> fmap Just x

io1 :: IO (Maybe a) -> R a
io1 x = R $ \_ -> x

lookup :: LookupHostPort -> R (Host,Port)
lookup = R

-- bricks

safeHead :: [a] -> R a
safeHead (x:_) = pure x
safeHead _ = empty

shuffle :: [x] -> R [x]
shuffle = io . shuffleM

decorate :: R a -> IO () -> R a
decorate g x = g <* io x
