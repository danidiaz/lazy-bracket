{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module LazyBracket where

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class

data Resource a = Resource
  { accessResource :: IO a,
    controlResource :: (a -> IO ()) -> IO ()
  }

lazyBracket :: (MonadIO m, MonadMask m) => IO a -> (a -> m ()) -> (Resource a -> m b) -> m b
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
  let lazyRelease _ exitCase = do
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
