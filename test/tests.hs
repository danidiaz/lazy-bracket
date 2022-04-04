{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import Data.IORef
import LazyBracket
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "All"
    [ testCase "doesAcquire" doesAcquire
    ]

data TestResourceState
  = NotYetAcquired
  | AlreadyAcquired
  | Disposed
  deriving (Show, Eq)

doesAcquire :: Assertion
doesAcquire = do
  ref <- newIORef NotYetAcquired
  let refMustBe expectedState = do
        testState <- readIORef ref
        assertEqual "resource state"  expectedState testState
  _ <-
    lazyBracket
      (writeIORef ref AlreadyAcquired)
      (\_ -> writeIORef ref Disposed)
      (\Resource {accessResource, controlResource} -> do 
          refMustBe NotYetAcquired
          _ <- accessResource
          refMustBe AlreadyAcquired
          _ <- accessResource
          refMustBe AlreadyAcquired
          pure ())
  refMustBe Disposed
  pure ()

main :: IO ()
main = defaultMain tests
