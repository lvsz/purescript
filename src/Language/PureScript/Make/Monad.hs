{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DictionaryApplications #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
module Language.PureScript.Make.Monad
  ( -- * Implementation of Make API using files on disk
    MakeIO(..)
  , MonadMake(..)
  , runMake
  , readTextFile
  ) where

import           Prelude

import           Control.Monad.Base (MonadBase(..))
import           Control.Monad.Error.Class (MonadError(..))
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader (MonadReader(..), ReaderT(..))
import           Control.Monad.Trans.Control (MonadBaseControl(..), defaultLiftBaseWith, defaultRestoreM)
import           Control.Monad.Trans.Except
import           Control.Monad.Writer.Class (MonadWriter(..))
import qualified Data.ByteString.Lazy as B
import           Language.PureScript.AST
import           Language.PureScript.Errors
import           Language.PureScript.Options
import           System.IO.Error (tryIOError)

import           Data.IORef
import GHC.Base (Functor(..), Applicative(..), Monad(..))
import GHC.Exts (Constraint)
import Data.Semigroup (Semigroup(..))
import Data.Monoid (Monoid(..))

import Control.Monad.Writer

class ( MonadReader Options m
      , MonadIO m
      , MonadBase IO m
      , MonadBaseControl IO m
      ) => MonadMakeBase m where
    makeIOBase :: (IOError -> ErrorMessage) -> IO a -> m a

class ( Monad m
      , MonadReader Options m
      , MonadWriter MultipleErrors m
      , MonadError MultipleErrors m
      , MonadIO m
      , MonadBase IO m
      , MonadBaseControl IO m
      ) => MonadMake m where
    makeIO :: (IOError -> ErrorMessage) -> IO a -> m a

-- makeImpIO :: IORef Options -> IORef MultipleErrors -> MonadMake.Dict IO
-- ioRefMakeBase :: IORef Options -> MonadMakeBase.Dict IO
-- ioRefMakeBase opts = MonadMakeBase.Dict
--     { parent1 = ioRefReader opts
--     , parent2 = getDict @(MonadIO IO)
--     , parent3 = getDict @(MonadBase IO IO)
--     , parent4 = getDict @(MonadBaseControl IO IO)
--     , makeIOBase = \f io -> do
--         e <- liftIO $ tryIOError io
--         either (undefined . singleError . f) return e
--     }

--newtype Make a = Make
  --{ unMake :: ExceptT MultipleErrors (ReaderT Options (Logger MultipleErrors)) a
  --} deriving (Functor, Applicative, Monad, MonadIO, MonadError MultipleErrors, MonadWriter MultipleErrors, MonadReader Options)
-- runIORefMakeBase :: forall m. MonadMakeBase m => IORef Options -> 
newtype MakeIO a = MakeIO
  { unMakeIO :: ExceptT MultipleErrors (LoggerT MultipleErrors IO) a
  } deriving ( Functor, Applicative, Monad, MonadIO, MonadError MultipleErrors, MonadWriter MultipleErrors)

instance MonadBase IO MakeIO where
  liftBase = liftIO

instance MonadBaseControl IO MakeIO where
  type StM MakeIO a = Either MultipleErrors a
  liftBaseWith f = MakeIO $ liftBaseWith $ \q -> f (q . unMakeIO)
  restoreM = MakeIO . restoreM


ioRefMake :: IORef Options -> MonadMake.Dict MakeIO
ioRefMake opts = MonadMake.Dict
    { parent1 = getDict @(Monad MakeIO)
    , parent2 = ioRefReader opts
    , parent3 = getDict @(MonadWriter MultipleErrors MakeIO)
    , parent4 = getDict @(MonadError MultipleErrors MakeIO)
    , parent5 = getDict @(MonadIO MakeIO)
    , parent6 = getDict @(MonadBase IO MakeIO)
    , parent7 = getDict @(MonadBaseControl IO MakeIO)
    , makeIO = \f io -> do
        e <- liftIO $ tryIOError io
        either (undefined . singleError . f) return e
    }

runIORefMake ::  Options ->
                 (forall m. MonadMake m => m a) ->
                  IO (Either MultipleErrors a, MultipleErrors)
runIORefMake opts m = do
    ref <- newIORef opts
    runLoggerT' $ runExceptT $ unMakeIO $ m (( ioRefMake ref ))

class HasDict (c :: Constraint) where
    type Dict c :: *
    getDict :: c => Dict c

instance HasDict (Functor f) where
    type Dict (Functor f) = Functor.Dict f
    getDict = Functor.Dict
        { fmap = fmap
        , (<$) = (<$)
        }

instance HasDict (Applicative f) where
    type Dict (Applicative f) = Applicative.Dict f
    getDict = Applicative.Dict
        { parent1 = getDict @(Functor f)
        , pure = pure
        , (<*>) = (<*>)
        , (<*) = (<*)
        , (*>) = (*>)
        , liftA2 = liftA2
        }

instance HasDict (Monad m) where
    type Dict (Monad m) = Monad.Dict m
    getDict = Monad.Dict
        { parent1 = getDict @(Applicative m)
        , (>>=) = (>>=)
        , (>>) = (>>)
        , return = return
        , fail = fail
        }

instance HasDict (Semigroup a) where
    type Dict (Semigroup a) = Semigroup.Dict a
    getDict = Semigroup.Dict
        { (<>) = (<>)
        , sconcat = sconcat
        , stimes = stimes
        }

instance HasDict (Monoid a) where
    type Dict (Monoid a) = Monoid.Dict a
    getDict = Monoid.Dict
        { parent1 = getDict @(Semigroup a)
        , mempty = mempty
        , mappend = mappend
        , mconcat = mconcat
        }

instance HasDict (MonadWriter w m) where
    type Dict (MonadWriter w m) = MonadWriter.Dict w m
    getDict = MonadWriter.Dict
        { parent1 = getDict @(Monoid w)
        , parent2 = getDict @(Monad m)
        , writer = writer
        , tell = tell
        , listen = listen
        , pass = pass
        }

instance HasDict (MonadError e m) where
    type Dict (MonadError e m) = MonadError.Dict e m
    getDict = MonadError.Dict
        { parent1 = getDict @(Monad m)
        , catchError = catchError
        , throwError = throwError
        }

instance HasDict (MonadIO m) where
    type Dict (MonadIO m) = MonadIO.Dict m
    getDict = MonadIO.Dict
        { parent1 = getDict @(Monad m)
        , liftIO = liftIO
        }

instance HasDict (MonadBase b m) where
    type Dict (MonadBase b m) = MonadBase.Dict b m
    getDict = MonadBase.Dict
        { parent1 = getDict @(Applicative b)
        , parent2 = getDict @(Applicative m)
        , parent3 = getDict @(Monad b)
        , parent4 = getDict @(Monad m)
        , liftBase = liftBase
        }

instance HasDict (MonadBaseControl b m) where
    type Dict (MonadBaseControl b m) = MonadBaseControl.Dict b m
    getDict = MonadBaseControl.Dict
        { parent1 = getDict @(MonadBase b m)
        , liftBaseWith = liftBaseWith
        , restoreM = restoreM
        }

ioRefReader ref = MonadReader.Dict
    { parent1 = getDict @(Monad MakeIO)
    , ask = liftIO $ readIORef ref
    , local = \f m -> do
        r <- liftIO $ readIORef ref
        liftIO $ writeIORef ref (f r)
        res <- m
        liftIO $ writeIORef ref r
        return res
    , reader = \f -> do
        r <- liftIO $ readIORef ref
        return $ f r
    }

--runReaderIO :: (forall m. MonadReader r m => m a) -> r -> IO a
--runReaderIO m r = do
    --ref <- newIORef r
    --m (( ioRefReader ref ))

{-
ioRefWriter :: Monoid r => IORef r -> MonadWriter.Dict r IO
ioRefWriter ref = MonadWriter.Dict
    { parent1 = Monoid.Dict { parent1 = Semigroup.Dict { (<>) = (<>), sconcat = sconcat, stimes = stimes }, mempty = mempty, mappend = mappend, mconcat = mconcat }
    , parent2 = getDict @(Monad IO)
    , writer = \(a, w) -> do
        writeIORef ref w
        return a
    , tell = \w -> modifyIORef' ref (flip mappend w)
    , listen = undefined -- \m -> do
        -- a@(_, w') <- m
        -- w <- flip mappend w' <$> readIORef ref
        -- writeIORef ref w
        -- return (a, w)
    , pass = undefined -- \m -> do
        -- ((a, f), w) <- m
        -- return (a, f w)
    }

runStateIO :: (forall m. MonadState state m => m a) -> state -> IO a

ioRefWriter' :: Monoid w => IORef w -> MonadWriter.Dict w IO
ioRefWriter' ref = MonadWriter.Dict
    { parent1 = undefined :: Monoid.Dict a
    , parent2 = getDict @(Monad IO)
    , writer = undefined :: Monoid w => (a, w) -> IO a
    , tell = undefined :: Monoid w => w -> IO ()
    , listen = undefined :: Monoid w => IO a -> IO (a, w)
    , pass = undefined :: Monoid w => m (a, w -> w) -> m a
    }

runWriterIO :: (Semigroup w, Monoid w) => (forall m. MonadWriter w m => m (a, w)) -> IO a
runWriterIO m = do
    ref <- newIORef mempty
    m (( ioRefWriter' ref ))
    -}

    {-
class ( Monad m
      , MonadReader Options m
      , MonadWriter MultipleErrors m
      , MonadError MultipleErrors m
      , MonadIO m
      ) => MonadMake m where
    makeIO :: (IOError -> ErrorMessage) -> IO a -> m a
    readTextFile :: FilePath -> m B.ByteString
    -}

ioRefStringWriter :: IORef MultipleErrors -> MonadWriter.Dict MultipleErrors IO
ioRefStringWriter ref = MonadWriter.Dict
    { parent1 = getDict @(Monoid MultipleErrors)
    , parent2 = getDict @(Monad IO)
    , writer = undefined
    , tell = \w -> atomicModifyIORef' ref (\w' -> (mappend w' w, ()))
    , listen = undefined
    , pass = undefined
    }

--runIORefLogger :: (forall m. MonadErrorLogger m => m a) -> IO (a, MultipleErrors)
--runIORefLogger m = do
    --r <- newIORef $ MultipleErrors []
    --a <- m (( ioRefLogger r ))
    --w <- readIORef r
    --return (a, w)

runWriterIO :: (forall m. MonadWriter MultipleErrors m => m (a, MultipleErrors)) -> IO (a, MultipleErrors)
runWriterIO m = do
    ref <- newIORef mempty
    m (( ioRefStringWriter ref ))


-- | A monad for running make actions
newtype Make a = Make
  { unMake :: ExceptT MultipleErrors (ReaderT Options (Logger MultipleErrors)) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadError MultipleErrors, MonadWriter MultipleErrors, MonadReader Options)

instance MonadBase IO Make where
  liftBase = liftIO

instance MonadBaseControl IO Make where
  type StM Make a = Either MultipleErrors a
  liftBaseWith f = Make $ liftBaseWith $ \q -> f (q . unMake)
  restoreM = Make . restoreM

instance MonadMake Make where
    makeIO = makeIO'

--instance MonadMakeBase m => MonadMake (ExceptT MultipleErrors (WriterT MultipleErrors m)) where
    --makeIO f io = do
        --e <- liftIO $ tryIOError io
        --either (undefined . singleError . f) return e

-- | Execute a 'Make' monad, returning either errors, or the result of the compile plus any warnings.
-- runMake :: Options -> Make a -> IO (Either MultipleErrors a, MultipleErrors)
runMake :: Options -> (forall make. MonadMake make => make a) -> IO (Either MultipleErrors a, MultipleErrors)
runMake = runIORefMake
--runMake opts = runLogger' . flip runReaderT opts . runExceptT . unMake

{-
runMake :: MonadMake m => Options -> m a -> IO (Either MultipleErrors a, MultipleErrors)
runMake opts m = do
    ref <- newIORef opts
    -- runWriterT . runExceptT . m (( ioRefMakeBase ref ))
    m' (( ioRefMakeBase ref ))
  where
    m' :: MonadMakeBase m => m (Either MultipleErrors a, MultipleErrors)
    m' = runWriterT . runExceptT $ Except m
    -}

    -- undefined -- m (( ioRefMakeBase ref ))

-- | Run an 'IO' action in the 'Make' monad, by specifying how IO errors should
-- be rendered as 'ErrorMessage' values.
makeIO' :: MonadMake make => (IOError -> ErrorMessage) -> IO a -> make a
makeIO' f io = do
  e <- liftIO $ tryIOError io
  either (throwError . singleError . f) return e

-- | Read a text file in the 'Make' monad, capturing any errors using the
-- 'MonadError' instance.
readTextFile :: MonadMake make => FilePath -> make B.ByteString
readTextFile path = makeIO (const (ErrorMessage [] $ CannotReadFile path)) $ B.readFile path
