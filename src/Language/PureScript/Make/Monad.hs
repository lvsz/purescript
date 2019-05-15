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

class ( Monad m
      , MonadReader Options m
      , MonadWriter MultipleErrors m
      , MonadError MultipleErrors m
      , MonadIO m
      , MonadBase IO m
      , MonadBaseControl IO m
      ) => MonadMake m where
    makeIO :: (IOError -> ErrorMessage) -> IO a -> m a

newtype MakeIO a = MakeIO
  { unMakeIO :: ExceptT MultipleErrors IO a
  } deriving ( Functor, Applicative, Monad, MonadIO, MonadError MultipleErrors)

instance MonadBase IO MakeIO where
  liftBase = liftIO

instance MonadBaseControl IO MakeIO where
  type StM MakeIO a = Either MultipleErrors a
  liftBaseWith f = MakeIO $ liftBaseWith $ \q -> f (q . unMakeIO)
  restoreM = MakeIO . restoreM


ioRefMake :: IORef Options -> IORef MultipleErrors -> MonadMake.Dict MakeIO
ioRefMake opts errs = MonadMake.Dict
    { parent1 = getDict @(Monad MakeIO)
    , parent2 = ioRefReader opts
    , parent3 = ioRefErrorLogger errs
    , parent4 = getDict @(MonadError MultipleErrors MakeIO)
    , parent5 = getDict @(MonadIO MakeIO)
    , parent6 = getDict @(MonadBase IO MakeIO)
    , parent7 = getDict @(MonadBaseControl IO MakeIO)
    , makeIO = \f io -> do
        e <- liftIO $ tryIOError io
        either (undefined . singleError . f) return e
    }

runIORefMake :: Options
                -> (forall m. MonadMake m => m a)
                -> IO (Either MultipleErrors a, MultipleErrors)
runIORefMake opts m = do
    optsRef <- newIORef opts
    errsRef <- newIORef mempty
    res <- runExceptT $ unMakeIO $ m (( ioRefMake optsRef errsRef ))
    errs <- readIORef errsRef
    return (res, errs)

ioRefReader :: IORef Options -> MonadReader.Dict Options MakeIO
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

ioRefErrorLogger :: IORef MultipleErrors -> MonadWriter.Dict MultipleErrors MakeIO
ioRefErrorLogger ref = MonadWriter.Dict
    { parent1 = getDict @(Monoid MultipleErrors)
    , parent2 = getDict @(Monad MakeIO)
    , writer = \(a, w) -> do
        liftIO $ atomicModifyIORef' ref (\w' -> (mappend w' w, a))
    , tell = \w -> liftIO $ atomicModifyIORef' ref (\w' -> (mappend w' w, ()))
    , listen = \m -> do
        w <- liftIO $ atomicModifyIORef' ref ((,) mempty) -- liftIO $ readIORef ref
        a <- m
        liftIO . atomicModifyIORef' ref $ \w' -> (mappend w w', (a, w'))
    , pass = \m -> do
        w <- liftIO $ atomicModifyIORef' ref ((,) mempty) -- liftIO $ readIORef ref
        (a, f) <- m
        liftIO . atomicModifyIORef' ref $ \w' -> (mappend w (f w'), a)
    }

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

-- | Execute a 'Make' monad, returning either errors, or the result of the compile plus any warnings.
runMake :: Options
           -> (forall make. MonadMake make => make a)
           -> IO (Either MultipleErrors a, MultipleErrors)
runMake = runIORefMake

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

-- | Dictionary utilities
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
