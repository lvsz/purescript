{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- A replacement for WriterT IO which uses mutable references.
--
module Control.Monad.Logger where

import Prelude.Compat

import Control.Monad (ap)
import Control.Monad.Base (MonadBase(..))
import Control.Monad.IO.Class
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.Writer.Class
import Control.Monad.Identity

import Data.IORef

-- | A replacement for WriterT IO which uses mutable references.
newtype LoggerT w m a = LoggerT { runLoggerT :: IORef w -> m a }

-- | Run a Logger computation, starting with an empty log.
runLoggerT' :: (Monoid w, MonadIO m) => LoggerT w m a -> m (a, w)
runLoggerT' l = do
  r <- liftIO $ newIORef mempty
  a <- runLoggerT l r
  w <- liftIO $ readIORef r
  return (a, w)

instance (Functor m) => Functor (LoggerT w m) where
  fmap f (LoggerT l) = LoggerT $ \r -> fmap f (l r)

instance (Monoid w, Monad m) => Applicative (LoggerT w m) where
  pure a = LoggerT . const $ pure a
  LoggerT mf <*> LoggerT mx = LoggerT $ \w -> do
    f <- mf w
    x <- mx w
    pure (f x)

instance (Monoid w, Monad m) => Monad (LoggerT w m) where
  return = pure
  LoggerT l >>= f = LoggerT $ \r -> l r >>= \a -> runLoggerT (f a) r

instance (Monoid w, MonadIO m) => MonadIO (LoggerT w m) where
  liftIO = LoggerT . const . liftIO

instance (Monoid w, MonadIO m) => MonadWriter w (LoggerT w m) where
  tell w = LoggerT $ \r -> liftIO . atomicModifyIORef' r $ \w' -> (mappend w' w, ())
  listen l = LoggerT $ \r -> do
    (a, w) <- runLoggerT' l
    liftIO . atomicModifyIORef' r $ \w' -> (mappend w' w, (a, w))
  pass l = LoggerT $ \r -> do
    ((a, f), w) <- runLoggerT' l
    liftIO . atomicModifyIORef' r $ \w' -> (mappend w' (f w), a)

instance (Monoid w, MonadIO m) => MonadBase IO (LoggerT w m) where
  liftBase = liftIO

instance (Monoid w, MonadBaseControl IO m, MonadIO m) => MonadBaseControl IO (LoggerT w m) where
  type StM (LoggerT w m) a = StM m a
  liftBaseWith f = LoggerT $ \r -> liftBaseWith $ \q -> f (q . flip runLoggerT r)
  restoreM = LoggerT . const . restoreM

-- | Non transformer version
type Logger w = LoggerT w IO

runLogger' :: (Monoid w) => Logger w a -> IO (a, w)
runLogger' = runLoggerT'

{-
newtype Logger w a = Logger { runLogger :: IORef w -> IO a }

runLogger' :: (Monoid w) => Logger w a -> IO (a, w)
runLogger' = runLoggerT'
  r <- newIORef mempty
  a <- runIdentity $ runLoggerT l r
  w <- readIORef r
  return (a, w)

instance Functor (Logger w) where
  fmap f (Logger l) = Logger $ \r -> fmap f (l r)

instance (Monoid w) => Applicative (Logger w) where
  pure = Logger . const . pure
  (<*>) = ap

instance (Monoid w) => Monad (Logger w) where
  return = pure
  Logger l >>= f = Logger $ \r -> l r >>= \a -> runLogger (f a) r

instance (Monoid w) => MonadIO (Logger w) where
  liftIO = Logger . const

instance (Monoid w) => MonadWriter w (Logger w) where
  tell w = Logger $ \r -> atomicModifyIORef' r $ \w' -> (mappend w' w, ())
  listen l = Logger $ \r -> do
    (a, w) <- liftIO (runLogger' l)
    atomicModifyIORef' r $ \w' -> (mappend w' w, (a, w))
  pass l = Logger $ \r -> do
    ((a, f), w) <- liftIO (runLogger' l)
    atomicModifyIORef' r $ \w' -> (mappend w' (f w), a)

instance (Monoid w) => MonadBase IO (Logger w) where
  liftBase = liftIO

instance (Monoid w) => MonadBaseControl IO (Logger w) where
  type StM (Logger w) a = a
  liftBaseWith f = Logger $ \r -> liftBaseWith $ \q -> f (q . flip runLogger r)
  restoreM = return
  -}
