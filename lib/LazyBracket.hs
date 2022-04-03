{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
module LazyBracket where

import Control.Monad.Catch
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class

data Resource a = Resource
  { accessResource :: IO a,
    controlResource :: (a -> IO ()) -> IO ()
  }

lazyBracket :: (MonadIO m, MonadMask m) => IO a -> (a -> m c) -> (Resource a -> m b) -> m b
lazyBracket acquire release = fmap fst . lazyGeneralBracket
    acquire
    (\a _ -> release a)

data ResourceState a = 
    NotYetAcquired (a -> IO ())
    | AlreadyAcquired a

lazyGeneralBracket :: forall m a b c.
  (MonadIO m, MonadMask m) =>
  IO a ->
  (a -> ExitCase b -> m c) ->
  (Resource a -> m b) ->
  m (b, c)
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
            join $
                modifyMVarMasked ref \case
                    NotYetAcquired pendingOperations -> do
                        pure (NotYetAcquired (pendingOperations <> operation), mempty)
                    resourceState@(AlreadyAcquired a) -> do
                        pure (resourceState, operation a)
    let lazyResource = Resource {accessResource, controlResource}
    generalBracket (pure lazyResource) _ _ 
