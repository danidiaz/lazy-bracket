{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

data TestOps = TestOpA | TestOpB deriving (Show, Eq)


doesAcquire :: Assertion
doesAcquire = do
  ref <- newIORef NotYetAcquired
  opsRef <- newIORef @[TestOps] []
  let refMustBe msg expectedState = do
        testState <- readIORef ref
        assertEqual msg expectedState testState
  let opsRefMustBe msg expectedOpsState = do
        opsState <- readIORef opsRef
        assertEqual msg expectedOpsState opsState
  _ <-
    lazyBracket
      (writeIORef ref AlreadyAcquired)
      (\_ -> writeIORef ref Disposed)
      (\Resource {accessResource, controlResource} -> do 
          refMustBe "not acquired at the beginning" NotYetAcquired
          controlResource (\_ -> modifyIORef opsRef (<> [TestOpA]))
          refMustBe "control op doesn't trigger acquisition" NotYetAcquired
          opsRefMustBe "control op not executed before acquisition" []
          _ <- accessResource
          refMustBe "access to resource triggers acquisition" AlreadyAcquired
          opsRefMustBe "access to resource triggers pending ops" [TestOpA]
          writeIORef opsRef []
          _ <- accessResource
          opsRefMustBe "re-accessing the resource doesn't re-execute delayed control ops" []
          controlResource (\_ -> modifyIORef opsRef (<> [TestOpB]))
          opsRefMustBe "control ops when already acquired execute immediately" [TestOpB]
          pure ())
  refMustBe "release function must run" Disposed
  pure ()

-- TODO:
-- test allocation only happens once
-- test skipping allocation + control ops
-- test exception-throwing 

main :: IO ()
main = defaultMain tests
