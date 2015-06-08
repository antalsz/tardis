{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | The data definition of a "TardisT"
-- as well as its primitive operations,
-- and straightforward combinators based on the primitives.
-- 
-- See Control.Monad.Tardis for the general explanation
-- of what a Tardis is and how to use it.
module Control.Monad.Trans.Tardis (
    -- * The Tardis monad transformer
    TardisT(..)
  , evalTardisT
  , execTardisT

    -- * The Tardis monad
  , Tardis
  , runTardis
  , evalTardis
  , execTardis

    -- * Primitive Tardis operations
  , tardis

  , getPast
  , getFuture
  , sendPast
  , sendFuture

    -- * Composite Tardis operations
  , modifyForwards
  , modifyBackwards

  , modifyForwards'
  , modifyBackwards'

  , getsPast
  , getsFuture

    -- * Other
  , noState

    -- * Manupulating the Tardis monad
  , mapTardis
  , mapBackwards
  , mapForwards
  , withTardis
  , withBackwards
  , withForwards
    
    -- * Manupulating the Tardis monad transformer
  , mapTardisT
  , withTardisT
  , withBackwardsT
  , withForwardsT

    -- * Lifting other operations
  , liftListen
  , liftPass
  , liftCatch
  , liftCallCC
  , liftCallCC'
  ) where

import Control.Arrow
import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Trans

-- For instances
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Error
import Control.Monad.Cont

-- Definition
-------------------------------------------------

-- | A TardisT is parameterized by two state streams:
-- a 'backwards-traveling' state and a 'forwards-traveling' state.
-- This library consistently puts the backwards-traveling state first
-- whenever the two are seen together.
newtype TardisT bw fw m a = TardisT
  { runTardisT :: (bw, fw) -> m (a, (bw, fw))
    -- ^ A TardisT is merely an effectful state transformation
  }

-- | Using a Tardis with no monad underneath
-- will prove to be most common use case.
-- Practical uses of a TardisT require that the
-- underlying monad be an instance of MonadFix,
-- but note that the IO instance of MonadFix
-- is almost certainly unsuitable for use with
-- Tardis code.
type Tardis bw fw = TardisT bw fw Identity

-- | A Tardis is merely a pure state transformation.
runTardis :: Tardis bw fw a -> (bw, fw) -> (a, (bw, fw))
runTardis m = runIdentity . runTardisT m


-- Helpers
-------------------------------------------------

-- | Run a Tardis, and discard the final state,
-- observing only the resultant value.
evalTardisT :: Monad m => TardisT bw fw m a -> (bw, fw) -> m a
evalTardisT t s = fst `liftM` runTardisT t s

-- | Run a Tardis, and discard the resultant value,
-- observing only the final state (of both streams).
-- Note that the 'final' state of the backwards-traveling state
-- is the state it reaches by traveling from the 'bottom'
-- of your code to the 'top'.
execTardisT :: Monad m => TardisT bw fw m a -> (bw, fw) -> m (bw, fw)
execTardisT t s = snd `liftM` runTardisT t s


-- | Run a Tardis, and discard the final state,
-- observing only the resultant value.
evalTardis :: Tardis bw fw a -> (bw, fw) -> a
evalTardis t = runIdentity . evalTardisT t

-- | Run a Tardis, and discard the resultant value,
-- observing only the final state (of both streams).
execTardis :: Tardis bw fw a -> (bw, fw) -> (bw, fw)
execTardis t = runIdentity . execTardisT t


-- | Some Tardises never observe the 'initial' state
-- of either state stream, so it is convenient
-- to simply hand dummy values to such Tardises.
-- 
-- > noState = (undefined, undefined)
noState :: (a, b)
noState = (undefined, undefined)


-- Instances
-------------------------------------------------

instance MonadFix m => Monad (TardisT bw fw m) where
  return x = tardis $ \s -> (x, s)
  m >>= f  = TardisT $ \ ~(bw, fw) -> do
    rec (x,  ~(bw'', fw' )) <- runTardisT m (bw', fw)
        (x', ~(bw' , fw'')) <- runTardisT (f x) (bw, fw')
    return (x', (bw'', fw''))

instance MonadFix m => Functor (TardisT bw fw m) where
  fmap = liftM

instance MonadFix m => Applicative (TardisT bw fw m) where
  pure = return
  (<*>) = ap

instance (MonadFix m, Alternative m) => Alternative (TardisT bw fw m) where
  empty   = TardisT $ \_ -> empty
  m <|> n = TardisT $ \s -> runTardisT m s <|> runTardisT n s

instance (MonadFix m, MonadPlus m) => MonadPlus (TardisT bw fw m) where
  mzero       = TardisT $ \_ -> mzero
  m `mplus` n = TardisT $ \s -> runTardisT m s `mplus` runTardisT n s

instance MonadTrans (TardisT bw fw) where
  lift m = TardisT $ \s -> do
    x <- m
    return (x, s)

instance MonadFix m => MonadFix (TardisT bw fw m) where
  mfix f = TardisT $ \s -> do
    rec (x, s') <- runTardisT (f x) s
    return (x, s')

instance (MonadFix m, MonadReader r m) => MonadReader r (TardisT bw fw m) where
  ask    = lift ask
  local  = mapTardisT . local
  reader = lift . reader

instance (MonadFix m, MonadWriter w m) => MonadWriter w (TardisT bw fw m) where
  writer = lift . writer
  tell   = lift . tell
  listen = liftListen listen
  pass   = liftPass   pass

instance (MonadFix m, MonadError e m) => MonadError e (TardisT bw fw m) where
  throwError = lift . throwError
  catchError = liftCatch catchError

instance (MonadFix m, MonadIO m) => MonadIO (TardisT bw fw m) where
  liftIO = lift . liftIO

-- | This instance is modeled after 'State''s instance, but I'll be the first to
-- admit I don't really understand it.
instance (MonadFix m, MonadCont m) => MonadCont (TardisT bw fw m) where
  callCC = liftCallCC' callCC

-- Basics
-------------------------------------------------

-- | From a stateful computation, construct a Tardis.
-- This is the pure parallel to the constructor "TardisT",
-- and is polymorphic in the transformed monad.
tardis :: Monad m => ((bw, fw) -> (a, (bw, fw))) -> TardisT bw fw m a
tardis f = TardisT $ \s -> return (f s)

-- | Retrieve the current value of the 'forwards-traveling' state,
-- which therefore came forwards from the past.
-- You can think of forwards-traveling state as traveling
-- 'downwards' through your code.
getPast :: Monad m => TardisT bw fw m fw
getPast = tardis $ \ ~(bw, fw)  -> (fw, (bw, fw))

-- | Retrieve the current value of the 'backwards-traveling' state,
-- which therefore came backwards from the future.
-- You can think of backwards-traveling state as traveling
-- 'upwards' through your code.
getFuture :: Monad m => TardisT bw fw m bw
getFuture = tardis $ \ ~(bw, fw)  -> (bw, (bw, fw))

-- | Set the current value of the 'backwards-traveling' state,
-- which will therefore be sent backwards to the past.
-- This value can be retrieved by calls to "getFuture"
-- located 'above' the current location,
-- unless it is overwritten by an intervening "sendPast".
sendPast :: Monad m => bw -> TardisT bw fw m ()
sendPast bw' = tardis $ \ ~(_bw, fw) -> ((), (bw', fw))

-- | Set the current value of the 'forwards-traveling' state,
-- which will therefore be sent forwards to the future.
-- This value can be retrieved by calls to "getPast"
-- located 'below' the current location,
-- unless it is overwritten by an intervening "sendFuture".
sendFuture :: Monad m => fw -> TardisT bw fw m ()
sendFuture fw' = tardis $ \ ~(bw, _fw) -> ((), (bw, fw'))


-- | Modify the forwards-traveling state
-- as it passes through from past to future.
modifyForwards :: MonadFix m => (fw -> fw) -> TardisT bw fw m ()
modifyForwards f = getPast >>= sendFuture . f

-- | Modify the backwards-traveling state
-- as it passes through from future to past.
modifyBackwards :: MonadFix m => (bw -> bw) -> TardisT bw fw m ()
modifyBackwards f = do
  rec
    sendPast (f x)
    x <- getFuture
  return ()


-- | Modify the forwards-traveling state /strictly/
-- as it passes through from past to future.
modifyForwards' :: MonadFix m => (fw -> fw) -> TardisT bw fw m ()
modifyForwards' f = getPast >>= (sendFuture $!) . f


-- | Modify the backwards-traveling state /strictly/
-- as it passes through from future to past.  /This is in general very
-- dangerous!/ Strictness is antithetical to the time-traveling nature of this
-- state.
modifyBackwards' :: MonadFix m => (bw -> bw) -> TardisT bw fw m ()
modifyBackwards' f = mdo
  sendPast $! f x
  x <- getFuture
  return ()


-- | Retrieve a specific view of the forwards-traveling state.
getsPast :: MonadFix m => (fw -> a) -> TardisT bw fw m a
getsPast f = fmap f getPast


-- | Retrieve a specific view of the backwards-traveling state.
getsFuture :: MonadFix m => (bw -> a) -> TardisT bw fw m a
getsFuture f = fmap f getFuture

{------------------------------------------------------------------------------}

-- | Map both the return value and both final states of a computation using the
-- given function.
--
-- * @'runTardis' ('mapTardis' f m) = f . 'runTardis' m@
mapTardis :: ((a, (bw,fw)) -> (b, (bw,fw))) -> Tardis bw fw a -> Tardis bw fw b
mapTardis f = mapTardisT (Identity . f . runIdentity)

-- | Map both the return value and final backwards state of a computation using
-- the given function (leaving the forwards state unchanged).  Like 'mapTardis'
-- but just for the backwards state.
mapBackwards :: ((a,bw) -> (b,bw)) -> Tardis bw fw a -> Tardis bw fw b
mapBackwards f = mapTardis $ \(a,(bw,fw)) -> (,fw) <$> f (a,bw)

-- | Map both the return value and final forwards state of a computation using
-- the given function (leaving the backwards state unchanged).  Like 'mapTardis'
-- but just for the forwards state.
mapForwards :: ((a,fw) -> (b,fw)) -> Tardis bw fw a -> Tardis bw fw b
mapForwards f = mapTardis $ \(a,(bw,fw)) -> (bw,) <$> f (a,fw)

-- | @'withTardis' f m@ executes the action @m@ on a pair of forwards and
-- backwards states modified by applying @f@.  See also 'withBackwards' and
-- 'withForwards'.
withTardis :: ((bw,fw) -> (bw,fw)) -> Tardis bw fw a -> Tardis bw fw a
withTardis = withTardisT

-- | @'withBackwards' f m@ executes the action @m@ on a backwards state modified
-- by 'f' and an unmodified forwards state.
withBackwards :: (bw -> bw) -> Tardis bw fw a -> Tardis bw fw a
withBackwards = withBackwardsT

-- | @'withForwards' f m@ executes the action @m@ on an unmodified backwards
-- state and a forwards state modified by 'f'.
withForwards :: (fw -> fw) -> Tardis bw fw a -> Tardis bw fw a
withForwards = withForwardsT

-- | Map both the return value and both final states of a computation using the
-- given function.  You can't implement @mapBackwardsT :: (m (a,bw) -> n (b,bw))
-- -> TardisT bw fw m a -> TardisT bw fw n b@ in general (due to the lack of
-- constraints on @m@ and @n@), and similarly for @mapForwardsT@.
--
-- * @'runTardisT' ('mapTardisT' f m) = f . 'runTardisT' m@
mapTardisT :: (m (a, (bw,fw)) -> n (b, (bw,fw))) -> TardisT bw fw m a -> TardisT bw fw n b
mapTardisT f m = TardisT $ f . runTardisT m

-- | @'withTardisT' f m@ executes the action @m@ on a pair of forwards and
-- backwards states modified by applying @f@.  See also 'withBackwardsT' and
-- 'withForwardsT'.
withTardisT :: ((bw,fw) -> (bw,fw)) -> TardisT bw fw m a -> TardisT bw fw m a
withTardisT f m = TardisT $ runTardisT m . f

-- | @'withBackwardsT' f m@ executes the action @m@ on a backwards state modified
-- by 'f' and an unmodified forwards state.
withBackwardsT :: (bw -> bw) -> TardisT bw fw m a -> TardisT bw fw m a
withBackwardsT = withTardisT . first

-- | @'withForwardsT' f m@ executes the action @m@ on an unmodified backwards
-- state and a forwards state modified by 'f'.
withForwardsT :: (fw -> fw) -> TardisT bw fw m a -> TardisT bw fw m a
withForwardsT = withTardisT . second

{------------------------------------------------------------------------------}

-- For backwards compatibility, we inline the following type synonyms from
-- @transformers-0.4.0.0@ and up (in "Control.Monad.Signatures"):
--
--   * @type Listen w m a = m a -> m (a,w)@
--   * @type Pass w m a = m (a, w -> w) -> m a@
--   * @type Catch e m a = m a -> (e -> m a) -> m a@
--   * @type CallCC m a b = ((a -> m b) -> m a) -> m a@
--
-- The "right" types for these operations are:
--
--   * @liftListen :: (Monad m) => Listen w m (a,(bw,fw)) -> Listen w (TardisT bw fw m) a@
--   * @liftPass :: (Monad m) => Pass w m (a,(bw,fw)) -> Pass w (TardisT bw fw m) a@
--   * @liftCatch :: Catch e m (a,(bw,fw)) -> Catch e (TardisT bw fw m) a@
--   * @liftCallCC :: CallCC m (a,(bw,fw)) (b,(bw,fw)) -> CallCC (TardisT bw fw m) a b@
--   * @liftCallCC' :: CallCC m (a,(bw,fw)) (b,(bw,fw)) -> CallCC (TardisT bw fw m) a b@


-- | Lift a @listen@ operation into the 'TardisT' transformer.
liftListen :: Monad m
           => (m (a,(bw,fw)) -> m ((a,(bw,fw)),w))
           -> (TardisT bw fw m a -> TardisT bw fw m (a,w))
liftListen listen' m = TardisT $ \s -> do
    ((a, s'), w) <- listen' $ runTardisT m s
    return ((a, w), s')

-- | Lift a @pass@ operation into the 'TardisT' transformer.
liftPass :: Monad m
         => (m ((a,(bw,fw)), w -> w) -> m (a,(bw,fw)))
         -> (TardisT bw fw m (a, w -> w) -> TardisT bw fw m a)
liftPass pass' m = TardisT $ \s -> pass' $ do
    ((a, f), s') <- runTardisT m s
    return ((a, s'), f)

-- | Lift a @catchE@ operation into the 'TardisT' transformer.
liftCatch :: (m (a,(bw,fw)) -> (e -> m (a,(bw,fw))) -> m (a,(bw,fw)))
          -> (TardisT bw fw m a -> (e -> TardisT bw fw m a) -> TardisT bw fw m a)
liftCatch catchE' m h =
    TardisT $ \s -> runTardisT m s `catchE'` \e -> runTardisT (h e) s

-- | Uniform (I think) lifting of a @callCC@ operation into the 'TardisT' transformer.
-- This version rolls back to the original states on entering the continuation.
-- (I'm not 100% sure this is the right implementation in the presence of time
-- travel, though.)
liftCallCC :: ((((a,(bw,fw)) -> m (b,(bw,fw))) -> m (a,(bw,fw))) -> m (a,(bw,fw)))
           -> (((a -> TardisT bw fw m b) -> TardisT bw fw m a) -> TardisT bw fw m a)
liftCallCC callCC' f = TardisT $ \s ->
    callCC' $ \c ->
    runTardisT (f (\a -> TardisT $ \_ -> c (a, s))) s

-- | In-situ lifting of a @callCC@ operation into the 'TardisT' transformer.
-- This version uses the current states on entering the continuation.  It does
-- not satisfy the uniformity property (I think; see
-- "Control.Monad.Signatures").  (I'm not 100% sure this is the right
-- implementation in the presence of time travel, though.)
liftCallCC' :: ((((a,(bw,fw)) -> m (b,(bw,fw))) -> m (a,(bw,fw))) -> m (a,(bw,fw)))
            -> (((a -> TardisT bw fw m b) -> TardisT bw fw m a) -> TardisT bw fw m a)
liftCallCC' callCC' f = TardisT $ \s ->
    callCC' $ \c ->
    runTardisT (f (\a -> TardisT $ \s' -> c (a, s'))) s
