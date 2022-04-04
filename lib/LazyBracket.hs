{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | This module provides variants of the 'bracket' function that delay the
-- acquisition of the resource until it's used for the first time. If
-- the resource is never used, it will never be acquired.
--
-- To be even more lazy, certain kinds of operations on the resource do not
-- trigger acquisition: instead, they are stashed and applied once the resource
-- has been acquired for other reasons.
module LazyBracket
  ( -- * Lazy brackets that delay resource acquisition.
    lazyBracket,
    lazyGeneralBracket,
    lazyGeneralBracket_,

    -- * Resource wrapper.
    Resource (..),

    -- * Re-exports.
    ExitCase (..),
  )
where

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class

-- | A wrapper type over resources that delays resource acquisition.
--
-- Because one must be careful with the kinds of functions that are passed to 'controlResource',
-- it might be a good idea to define convenience wrappers over 'Resource' with
-- more restricted interfaces.
data Resource a = Resource
  { -- | Action to get hold of the resource. Will trigger resource acquisition
    -- and apply all stashed control operations the first time it's run.
    accessResource :: IO a,
    -- | Immediately apply a \"control\" operation to the underlying resource if
    -- the resource has already been acquired, otherwise stash the operation
    -- with the intention of applying it once the resource is eventually acquired.
    -- If the resource is never acquired, stashed operations are discarded.
    --
    -- By \"control\" operations we mean operations that are not essential in and of
    -- themselves, only serve to modify the behaviour of operations that are actually
    -- essential, and can be omitted if no essential operations take place.
    --
    -- Some examples:
    --
    -- For file handle resources, @hSetBuffering@ is a valid control
    -- operation, whereas actually writing bytes to the handle is not.
    --
    -- For database connection resources, beginning a transaction is a valid control
    -- operation, whereas performing an INSERT is not.
    controlResource :: (a -> IO ()) -> IO ()
  }

-- | A version of 'Contro.Monad.Catch.bracket' for which the resource is not
-- acquired at the beginning, but the first time it's used in the main callback.
-- If the resource is never used, it won't be acquired.
lazyBracket ::
  (MonadIO m, MonadMask m) =>
  -- | Computation to run to acquire the resource.
  IO a ->
  -- | Computation to run to release the resource, in case it was acquired.
  (a -> m c) ->
  -- | Computation to run in-between (might trigger resource acquisition).
  (Resource a -> m b) ->
  -- | Returns the value from the in-between computation
  m b 
lazyBracket acquire release action = do
  lazyGeneralBracket_
    acquire
    (\a _ -> release a)
    action

data ResourceState a
  = NotYetAcquired (a -> IO ())
  | AlreadyAcquired a

-- | A version of 'Contro.Monad.Catch.generalBracket' for which the resource is not
-- acquired at the beginning, but the first time it's used in the main callback.
-- If the resource is never used, it won't be acquired.
--
lazyGeneralBracket ::
  forall m a b c.
  (MonadIO m, MonadMask m) =>
  -- | Computation to run to acquire the resource
  IO a ->
  -- | Computation to run to release the resource, in case it was acquired
  --
  -- The release function has knowledge of how the main callback was exited: by
  -- normal completion, by a runtime exception, or otherwise aborted.
  -- This can be useful when acquiring resources from resource pools,
  -- to decide whether to return the resource to the pool or to destroy it.
  (a -> ExitCase b -> m c) ->
  -- | Computation to run in-between (might trigger resource acquisition)
  (Resource a -> m b) ->
  -- | Returns the value from the in-between computation, and also of the
  -- release computation, if it took place.
  m (b, Maybe c)
lazyGeneralBracket acquire release action = do
  ref <- liftIO $ newMVar @(ResourceState a) (NotYetAcquired mempty)
  let accessResource = do
        (resource, pendingOperations) <- do
          modifyMVarMasked ref \case
            NotYetAcquired pendingOperations -> do
              resource <- acquire
              pure (AlreadyAcquired resource, (resource, pendingOperations))
            resourceState@(AlreadyAcquired a) -> do
              pure (resourceState, (a, mempty))
        pendingOperations resource -- no need to perform these inside the mask
        pure resource
  let controlResource operation = do
        runNow <- do
          modifyMVarMasked ref \case
            NotYetAcquired pendingOperations -> do
              pure (NotYetAcquired (pendingOperations <> operation), mempty)
            resourceState@(AlreadyAcquired a) -> do
              pure (resourceState, operation a)
        runNow
  let lazyResource = Resource {accessResource, controlResource}
  -- We ignore the 'Resource' argument because we extract the unwrapped
  -- version from the 'MVar'.
  let lazyRelease (_ :: Resource a) exitCase = do
        action <- liftIO $ do
          -- we don't mask here, already provided by generalBracket
          modifyMVar ref \case
            NotYetAcquired _ -> do
              pure (NotYetAcquired mempty, \_ -> pure Nothing)
            AlreadyAcquired a -> do
              pure (NotYetAcquired mempty, fmap Just <$> release a)
        -- If we ran this inside the modifyMVar, an exception during release
        -- would prevent resetting the state to NotYetAcquired. Do we want that?
        action exitCase
  generalBracket (pure lazyResource) lazyRelease action


-- | Slightly simpler version of 'lazyGeneralBracket' that doesn't return the result of the 
-- release computation.
lazyGeneralBracket_ ::
  forall m a b c.
  (MonadIO m, MonadMask m) =>
  -- | computation to run to acquire the resource
  IO a ->
  -- | computation to run to release the resource, in case it was acquired
  (a -> ExitCase b -> m c) ->
  -- | computation to run in-between (might trigger resource acquisition)
  (Resource a -> m b) ->
  -- | returns the value from the in-between computation.
  m b
lazyGeneralBracket_ acquire release action = do
  (b, _) <-
    lazyGeneralBracket
      acquire
      release
      action
  pure b
