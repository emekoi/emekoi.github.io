---
title: Complete and Easy
published: 2024-11-05
slug: complete-and-easy
---

::: {.details}
Haskell language extensions and module imports.

``` haskell
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Index ( main ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Function
import           Data.Kind
import           System.IO              (IO, putStrLn)
import           Data.String
```
:::

``` haskell
-- | The type of effects.
type Effect = Type -> Type

-- | Effect handlers.
newtype Handler (e :: Effect)
  = Handler { runHandler :: forall x. e x -> IO x }

-- | Scoped effect handlers.
newtype Scoped s (e :: Effect)
  = Scoped (forall x. e x -> IO x)

-- | Representaion of installed effect handlers as lists indexed by effects.
data Handlers (es :: [Effect]) where
  HNil :: Handlers '[]
  (:&) :: Handler e -> Handlers es -> Handlers (e ': es)

-- | A typeclass for looking up and masking handlers in the call stack.
class Handles e hs where
  getHandler :: Handlers hs -> Handler e
  maskHandler :: Handler e -> Handlers hs -> Handlers hs

-- | Base case for handler lookup.
instance {-# OVERLAPPING #-} Handles e (e ': es) where
  getHandler (h :& _) = h
  maskHandler h (_ :& hs) = h :& hs

-- | Recursive case for handler lookup.
instance Handles e es => Handles e (e' ': es) where
  getHandler (_ :& hs) = getHandler hs
  maskHandler h (h' :& hs) = h' :& maskHandler h hs

-- | The monad representing effects.
newtype Eff (es :: [Effect]) (x :: Type)
  = Eff { runEff :: Handlers es -> IO x }

instance Handles IO es => MonadIO (Eff es) where
  liftIO = Eff . const

-- | Lift an IO action into Eff.
unsafeLiftIO :: IO a -> Eff es a
unsafeLiftIO = Eff . const

-- | Run an Eff action consisting of completely handled effects.
run :: Eff '[] a -> IO a
run (Eff m) = m HNil

-- | Run an Eff action making use of raw IO.
runIO :: Eff '[IO] a -> IO a
runIO (Eff m) = m (Handler id :& HNil)

-- | Boilerplate instance.
instance Functor (Eff es) where
  fmap f (Eff r) = Eff (fmap f . r)
  {-# INLINE fmap #-}

-- | Boilerplate instance.
instance Applicative (Eff es) where
  pure x = Eff \_ -> pure x
  {-# INLINE pure #-}

  Eff mf <*> Eff mx = Eff \hs -> mf hs <*> mx hs
  {-# INLINE (<*>) #-}

  liftA2 f (Eff ma) (Eff mb) = Eff \hs -> liftA2 f (ma hs) (mb hs)
  {-# INLINE liftA2 #-}

-- | Boilerplate instance.
instance Monad (Eff es) where
  Eff m >>= k = Eff \hs -> m hs >>= flip runEff hs . k
  {-# INLINE (>>=) #-}

-- | Perform an effect.
perform :: Handles e es => e x -> Eff es x
perform e = Eff \hs -> runHandler (getHandler hs) e
{-# INLINE perform #-}

-- | Perform an effect.
performScoped :: Scoped s e -> e x -> Eff es x
performScoped (Scoped h) e = Eff \_ -> h e
{-# INLINE performScoped #-}

-- | Handle an effect.
handle :: (forall x. e x -> Eff es x) -> Eff (e ': es) a -> Eff es a
handle h (Eff m) = Eff \hs -> m (Handler (flip runEff hs . h) :& hs)
{-# INLINE handle #-}

withScoped :: Scoped s e -> Eff (e ': es) a -> Eff es a
withScoped (Scoped h) (Eff m) = Eff \hs -> m (Handler h :& hs)
{-# INLINE withScoped #-}

scoped :: (forall x. e x -> Eff es x) -> (forall s. Scoped s e -> Eff es a) -> Eff es a
scoped h m = Eff \hs -> runEff (m (Scoped (flip runEff hs . h))) hs
{-# INLINE scoped #-}

-- | Lift an effectful action into an new handler context.
lift :: Eff es x -> Eff (e ': es) x
lift (Eff m) = Eff \(_ :& hs) -> m hs
{-# INLINE lift #-}

-- | Re-handle an effect with the same handler.
subsume :: Handles e es => Eff (e ': es) x -> Eff es x
subsume (Eff m) = Eff \hs -> m (getHandler hs :& hs)
{-# INLINE subsume #-}

-- | Handle an effect by transforming it into another effect.
trans :: (forall x. e x -> Eff (e' ': es) x) -> Eff (e ': es) a -> Eff (e' ': es) a
trans h (Eff m) = Eff \hs@(_ :& hs') -> m (Handler (flip runEff hs . h) :& hs')
{-# INLINE trans #-}

-- | Replace the handler for an effect.
mask :: Handles e es => (forall x. e x -> Eff es x) -> Eff es a -> Eff es a
mask h (Eff m) = Eff \hs -> m (maskHandler (Handler (flip runEff hs . h)) hs)
{-# INLINE mask #-}
```

``` haskell
data Logging :: Effect where
  Log :: String -> Logging ()

log :: Handles Logging es => String -> Eff es ()
log = perform . Log

runLogging1 :: Eff (Logging ': es) a -> Eff es a
runLogging1 = handle \(Log msg) ->
  unsafeLiftIO $ putStrLn msg

runLogging2 :: Handles IO es => Eff (Logging ': es) a -> Eff es a
runLogging2 = handle \(Log msg) ->
  liftIO $ putStrLn msg

withLogger :: Handles IO es => (forall s. Scoped s Logging -> Eff es a) -> Eff es a
withLogger = scoped \(Log msg) ->
  liftIO $ putStrLn msg

main :: IO ()
main = do
  run $ runLogging1 do
    log "Hello World"

  runIO $ runLogging2 do
    withLogger \logger ->
      performScoped logger (Log "xxx")

    log "Goodbye World"
```
