---
title: Effects Handlers
published: 2025-02-04
slug: effect-handlers
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
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

module Index ( module Index ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Function
import           Data.Kind
import           System.IO              (IO, putStrLn)
import           Data.String
import           Data.IORef
import           Data.Int
```
:::

First and foremost, this post is *heavily* inspired by [Xiaoyan Ren](https://dayli.ly/)'s' post on
effect handlers via evidence passing[^1]. The basic idea is that you use an extensible record
(type-indexed list) as an evidence vector to track which handlers are in scope, alongside the
handlers themselves. When you want to perform an effect, you perform a runtime search through the
evidence vector to find the right handler.

[^1]: http://web.archive.org/web/20230223102349/https://xn--i2r.xn--rhqv96g/2022/02/03/readert-is-extensible-effects/

``` haskell
type Effect = Type -> Type
```

Effects are represented by indexed types, which allow up to create families of effects that are
group together under a single handler. (NOTE: is this true?)

``` haskell
newtype Handler (e :: Effect)
  = Handler { runHandler :: forall x. e x -> IO x }
```

Effect handlers are natural transformations between a given effect and `IO`. However, in order to
support a notion of scoped effects we introduce another type of handler with a phantom type
parameter[^2]. This type parameter is what lets enforce proper use of these handlers...

[^2]: I'm not sure if my use of "scoped" agrees with those in you'll find in other descriptions of effect systems.

``` haskell
data Handlers (es :: [Effect]) where
  HNil :: Handlers '[]
  (:&) :: Handler e -> Handlers es -> Handlers (e ': es)

class Handles (e :: Effect) (hs :: [Effect]) where
  getHandler :: Handlers hs -> Handler e
  replaceHandler :: Handler e -> Handlers hs -> Handlers hs
```

Since Haskell doesn't have extensible records, we have to fake them with lists indexed by types.
However, to actually do anything useful with these lists we have to use typeclasses. `Handles`
defines two methods:

::: {.dl}
`getHandler`

Given an evidence vector `es` containing at least one `Handler e`, retrieve the innermost one.

`replaceHandler`

Given an evidence vector `es` containing at least one `Handler e`, and new `Handler e` replace the
innermost `Handler e`.

:::

`getHandler` is self-explanatory. `replaceHandler` exists to allow use to replace a previously
installed handler with a new handler at the same-depth.

``` haskell
instance {-# OVERLAPPING #-} Handles e (e ': es) where
  getHandler (h :& _) = h
  {-# INLINE getHandler #-}

  replaceHandler h (_ :& hs) = h :& hs
  {-# INLINE replaceHandler #-}

instance Handles e es => Handles e (e' ': es) where
  getHandler (_ :& hs) = getHandler hs
  {-# INLINE getHandler #-}

  replaceHandler h (h' :& hs) = h' :& replaceHandler h hs
  {-# INLINE replaceHandler #-}
```

Looking closely, you might realize that these are just a restricted form of monomorphic lenses for
`Handles`.

We can then define our effect monad, `Eff` (which is morally `ReaderT (Handlers es) IO`) with some
functions for creating `Eff` actions from `IO` and running `Eff` actions.

``` haskell
newtype Eff (es :: [Effect]) (x :: Type)
  = Eff { runEff :: Handlers es -> IO x }

unsafeLiftIO :: IO a -> Eff es a
unsafeLiftIO = Eff . const
{-# INLINE unsafeLiftIO #-}

run :: Eff '[] a -> IO a
run (Eff m) = m HNil

runIO :: Eff '[IO] a -> IO a
runIO (Eff m) = m (Handler id :& HNil)

instance Handles IO es => MonadIO (Eff es) where
  liftIO = Eff . const
  {-# INLINE liftIO #-}
```

Haskell can't derive the `Applicative` and `Monad` instances for `Eff`, and since we aren't using
`transformers`, we have to write them all out by hand.

::: {.details}
Boilerplate instances.

``` haskell
instance Functor (Eff es) where
  fmap f (Eff r) = Eff (fmap f . r)
  {-# INLINE fmap #-}

instance Applicative (Eff es) where
  pure x = Eff \_ -> pure x
  {-# INLINE pure #-}

  Eff mf <*> Eff mx = Eff \hs -> mf hs <*> mx hs
  {-# INLINE (<*>) #-}

  liftA2 f (Eff ma) (Eff mb) = Eff \hs -> liftA2 f (ma hs) (mb hs)
  {-# INLINE liftA2 #-}

instance Monad (Eff es) where
  Eff m >>= k = Eff \hs -> m hs >>= flip runEff hs . k
  {-# INLINE (>>=) #-}
```

:::

Now we can finally start writing some interesting code. Performing effects is straightforward. We
lookup the handler and use it to handle the effect:

``` haskell
perform :: Handles e es => e x -> Eff es x
perform e = Eff \hs -> runHandler (getHandler hs) e
{-# INLINE perform #-}
```

Handling an effect takes a handling function and adds it to the list of handlers:

``` haskell
mkHandler :: (forall x. e x -> Eff es x) -> Handlers es -> Handler e
mkHandler h hs = Handler (flip runEff hs . h)
{-# INLINE mkHandler #-}

handle :: (forall x. e x -> Eff es x) -> Eff (e ': es) a -> Eff es a
handle h (Eff m) = Eff \hs -> m (mkHandler h hs :& hs)
{-# INLINE handle #-}
```

We can also handle effects by turning them into other effects:

``` haskell
handleAs :: (forall x. e x -> Eff (e' ': es) x) -> Eff (e ': es) a -> Eff (e' ': es) a
handleAs h (Eff m) = Eff \hs@(_ :& hs') -> m (mkHandler h hs :& hs')
{-# INLINE handleAs #-}
```

We can also replace an existing effect handler with a different one:

``` haskell
handleWith :: Handles e es => (forall x. e x -> Eff es x) -> Eff es a -> Eff es a
handleWith h (Eff m) = Eff \hs -> m (replaceHandler (mkHandler h hs) hs)
{-# INLINE handleWith #-}
```

We can also handle effects by using effect handlers already in scope:

``` haskell
subsume :: Handles e es => Eff (e ': es) x -> Eff es x
subsume (Eff m) = Eff \hs -> m (getHandler hs :& hs)
{-# INLINE subsume #-}
```

Effects handlers also come with a notion of "lifting", where effectful computations are injected
into a larger effect context without actually performing any of these new additional effects:

``` haskell
lift :: Eff es x -> Eff (e : es) x
lift (Eff m) = Eff \(_ :& hs) -> m hs
{-# INLINE lift #-}
```

It's probably possible to define a more general lifting function from `Eff es x` to `Eff (es' ++ es)
x`, but I don't know how to write such a function. It will probably have to use type families or
some equally powerful feature.


# Scoped Effects

To apply scoped effects, since we already have the handler we call it directly.


``` haskell
newtype Scoped s (e :: Effect)
  = Scoped (Handler e)

handleScoped :: (forall s x. e s x -> Eff es x) -> (forall s. Scoped s (e s) -> Eff (e s ': es) a) -> Eff es a
handleScoped hf m = Eff \hs -> do
  let h = mkHandler hf hs
  runEff (m (Scoped h)) (h :& hs)
{-# INLINE handleScoped #-}
```

``` haskell
performScoped :: Scoped s e -> e x -> Eff es x
performScoped (Scoped h) e = Eff \_ -> runHandler h e
{-# INLINE performScoped #-}
```

If we have a scoped handler, we can run effectful computations with them installed like regular handlers:

``` haskell
withScoped :: Scoped s e -> Eff (e ': es) a -> Eff es a
withScoped (Scoped h) (Eff m) = Eff (m . (h :&))
{-# INLINE withScoped #-}
```

What sets scoped handlers apart from regular handlers is how they are created. Like with `runST`, we
use a rank 2 polymorphic type in order to ensure the handler and any data we may associate with it
do not escape.

``` haskell
scoped :: (forall x. e x -> Eff es x) -> (forall s. Scoped s e -> Eff es a) -> Eff es a
scoped h m = Eff \hs -> runEff (m (Scoped (mkHandler h hs))) hs
{-# INLINE scoped #-}
```

# Examples

Following the article this was based on, we can define a `Logging` effect.

``` haskell
data Logging :: Effect where
  Log :: String -> Logging ()

log :: Handles Logging es => String -> Eff es ()
log = perform . Log
```

We can interpret this effect in two different ways. As a scoped effect (capability):

``` haskell
withLogger :: (forall s. Scoped s Logging -> Eff es a) -> Eff es a
withLogger = scoped \(Log msg) ->
  unsafeLiftIO $ putStrLn msg
```

Or as an unscoped, global effect:

``` haskell
runLogging :: Eff (Logging ': es) a -> Eff es a
runLogging = handle \(Log msg) ->
  unsafeLiftIO $ putStrLn msg

exampleLogging :: Eff es ()
exampleLogging = runLogging do
  log "Hello World"

  withLogger \logger ->
    performScoped logger (Log "Scoped Logging")

  log "Goodbye World"
```

``` haskell
data Reader s x :: Effect where
  Read :: Reader s x x
  -- GetName :: Reader s x (Name s Reader)

runReader :: x -> (forall s. Eff (Reader s x ': es) a) -> Eff es a
runReader v m = handle (\Read -> pure v) m
```

A more compelling usecase for scoped effects is local references:

-- performScoped :: Scoped s e -> e x -> Eff es x
-- withScoped :: Scoped s e -> Eff (e ': es) a -> Eff es a
-- scoped :: (forall x. e x -> Eff es x) -> (forall s. Scoped s e -> Eff es a) -> Eff es a

``` haskell
-- data Heap :: Effect where
--   NewRef :: Int -> Heap (Ref Int)

```

``` haskell
-- newtype Ref s x = Ref (IORef x)

-- data Ref :: Effect where
--   -- Local :: x -> Local (Ref s x)
--   Get :: Ref Int
--   Put :: Int -> Ref ()

-- data Heap :: Effect where
--   NewRef :: Int -> Heap (Ref Int)

-- newRef :: Handles (Heap s) es => x -> Eff es (Ref s x)
-- newRef = perform . NewRef

-- ref :: Int -> ((Scoped s Ref -> Eff es a) -> Eff es a) -> Eff es a
-- ref v k = do
--   r <- unsafeLiftIO (newIORef v)
--   scoped
--     (\case
--         Get -> unsafeLiftIO (readIORef r)
--         Put x -> unsafeLiftIO (writeIORef r x)
--     ) k

  
-- runHeap :: (forall s. Eff (Heap s : es) a) -> Eff es a
-- runHeap k =
--   handle (\(NewRef x) -> ref x _) k
  
-- local :: x -> Eff es (Ref s x, Scoped s (Local s))
-- local x = do
--   r <- unsafeLiftIO (newIORef x)

--   pure (Ref r, )

-- read :: Scoped s (Local) -> Ref s x -> x
-- read c r = _
```



``` haskell
main :: IO ()
main = run do
  exampleLogging
```
