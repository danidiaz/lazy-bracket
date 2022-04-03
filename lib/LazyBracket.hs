{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module LazyBracket
  ( 
    -- * Lazy brackets.
    lazyBracket,
    lazyGeneralBracket,
    -- * Resource wrapper for lazy acquisition.
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
data Resource a = Resource
  { -- | Action to get hold of the resource. Will trigger resource acquisition 
    -- and apply all stashed control operations the first time it's run.
    accessResource :: IO a,
    -- | Immediately apply a \"control\" operation to the underlying resource if
    -- the resource has already been acquired, otherwise stash the operation in
    -- with the intention of applying it once the resource is eventually acquired.
    -- If the resource is never acquired, stashed actions are discarded.
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

lazyBracket :: (MonadIO m, MonadMask m) => IO a 
  -- ^ computation to run to acquire the resource
  -> (a -> m ()) 
  -- ^ computation to run to release the resource (if the resource has been acquired)
  -> (Resource a -> m b) 
  -- ^ computation to run in-between (might trigger resource acquisition)
  -> m b -- returns the value from the in-between computation
lazyBracket acquire release =
  lazyGeneralBracket
    acquire
    (\a _ -> release a)

data ResourceState a
  = NotYetAcquired (a -> IO ())
  | AlreadyAcquired a

lazyGeneralBracket ::
  forall m a b.
  (MonadIO m, MonadMask m) =>
  IO a ->
  (a -> ExitCase b -> m ()) ->
  (Resource a -> m b) ->
  m b
lazyGeneralBracket acquire release action = do
  ref <- liftIO $ newMVar @(ResourceState a) (NotYetAcquired mempty)
  let accessResource = do
        (resource, operations) <- do
          modifyMVarMasked ref \case
            NotYetAcquired pendingOperations -> do
              resource <- acquire
              pure (AlreadyAcquired resource, (resource, pendingOperations))
            resourceState@(AlreadyAcquired a) -> do
              pure (resourceState, (a, mempty))
        operations resource -- no need to perform these inside the mask
        pure resource
  let controlResource operation = do
        action <- do
          modifyMVarMasked ref \case
            NotYetAcquired pendingOperations -> do
              pure (NotYetAcquired (pendingOperations <> operation), mempty)
            resourceState@(AlreadyAcquired a) -> do
              pure (resourceState, operation a)
        action
  let lazyResource = Resource {accessResource, controlResource}
  -- We ignore the 'Resource' argument because we extract the unwrapped
  -- version from the 'MVar'.
  let lazyRelease (_ :: Resource a) exitCase = do
        action <- liftIO $ do
          -- we don't mask here, already provided by generalBracket
          modifyMVar ref \case
            NotYetAcquired _ -> do
              pure (NotYetAcquired mempty, \_ -> pure ())
            AlreadyAcquired a -> do
              pure (NotYetAcquired mempty, release a)
        action exitCase
  (b, ()) <- generalBracket (pure lazyResource) lazyRelease action
  pure b
