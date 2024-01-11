{-# LANGUAGE DerivingVia #-}

module Util
    ( IORef
    , Trivial (..)
    , TrivialEq (..)
    , TrivialOrd (..)
    , foldM2
    , foldr2
    , modifyIORef
    , modifyIORef'
    , newIORef
    , readIORef
    , writeIORef
    ) where

import Control.Monad.IO.Class
import Data.IORef             (IORef)
import Data.IORef             qualified as IORef
import GHC.Exts               (oneShot)

newtype RecFold a b
  = MkRecFold (a -> (RecFold a b -> b) -> b)

pattern RecFold :: (a -> (RecFold a b -> b) -> b) -> RecFold a b
pattern RecFold f <- MkRecFold f
  where RecFold f = MkRecFold (oneShot f)
{-# COMPLETE RecFold #-}

foldr2 :: (Foldable f, Foldable g) => (a -> b -> c -> c) -> c -> f a -> g b -> c
foldr2 c i xs =
  foldr f (const i) xs . RecFold . foldr g (\_ _ -> i) where
    g e2 r2 e1 r1 = c e1 e2 (r1 (RecFold r2))
    f e r (RecFold f) = f e r
{-# INLINEABLE foldr2 #-}

foldM2
  :: (Monad m, Foldable f, Foldable g)
  => (c -> a -> b -> m c) -> c -> f a -> g b -> m c
foldM2 f z0 xs ys = foldr2 c return xs ys z0
  where c x y k z = f z x y >>= k; {-# INLINE c #-}
{-# INLINEABLE foldM2 #-}

-- | `Data.IORef.newIORef` lifted to `MonadIO`
newIORef :: (MonadIO m) => x -> m (IORef x)
newIORef = liftIO . IORef.newIORef

-- | `Data.IORef.readIORef` lifted to `MonadIO`
readIORef :: (MonadIO m) => IORef x -> m x
readIORef = liftIO . IORef.readIORef

-- | `Data.IORef.writeIORef` lifted to `MonadIO`
writeIORef :: (MonadIO m) => IORef x -> x -> m ()
writeIORef r = liftIO . IORef.writeIORef r

-- | `Data.IORef.modifyIORef` lifted to `MonadIO`
modifyIORef' :: (MonadIO m) => IORef x -> (x -> x) -> m ()
modifyIORef' k = liftIO . IORef.modifyIORef k

-- | `Data.IORef.modifyIORef'` lifted to `MonadIO`
modifyIORef :: (MonadIO m) => IORef x -> (x -> x) -> m ()
modifyIORef k = liftIO . IORef.modifyIORef' k

newtype TrivialEq r
  = TrivialEq r
  deriving (Show)
    via r
  deriving (Foldable, Functor, Traversable)

instance Eq (TrivialEq r) where
  (==) _ _ = True

instance Ord (TrivialEq r) where
  compare _ _ = EQ

newtype TrivialOrd r
  = TrivialOrd r
  deriving (Show)
    via r
  deriving (Eq, Foldable, Functor, Traversable)

instance (Eq r) => Ord (TrivialOrd r) where
  compare _ _ = EQ

newtype Trivial r
  = Trivial r
  deriving (Foldable, Functor, Traversable)

instance Eq (Trivial r) where
  (==) _ _ = True

instance Ord (Trivial r) where
  compare _ _ = EQ

instance Show (Trivial a) where
  show _ = "<trivial>"
