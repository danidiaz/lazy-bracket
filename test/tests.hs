{-# LANGUAGE BlockArguments #-}
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

import Control.Exception
import Data.IORef
import LazyBracket
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "All"
    [ testCase "doesAcquire" doesAcquire,
      testCase "doesAcquireEx" doesAcquireEx,
      testCase "doesNotAcquireTwice" doesNotAcquireTwice,
      testCase "noAcquisition" noAcquisition,
      testCase "noAcquisitionEx" noAcquisitionEx
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
      ( \Resource {accessResource, controlResource} -> do
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
          pure ()
      )
  refMustBe "release function must run" Disposed
  pure ()

data DummyEx = DummyEx deriving (Show, Eq)
instance Exception DummyEx

doesAcquireEx :: Assertion
doesAcquireEx = do
  ref <- newIORef NotYetAcquired
  opsRef <- newIORef @[TestOps] []
  let refMustBe msg expectedState = do
          testState <- readIORef ref
          assertEqual msg expectedState testState
  e <- try @DummyEx do
    _ <-
        lazyBracket
        (writeIORef ref AlreadyAcquired)
        (\_ -> writeIORef ref Disposed)
        ( \Resource {accessResource, controlResource} -> do
            _ <- accessResource
            throwIO DummyEx
            pure ()
        )
    pure ()
  case e of 
      Left DummyEx -> do
        refMustBe "release function must run" Disposed
        pure ()
      Right _ -> assertFailure "Exception should have bubbled up"


doesNotAcquireTwice :: Assertion
doesNotAcquireTwice = do
  let bombs = pure () : repeat (throwIO $ userError "boom!")
  bombsRef <- newIORef @[IO ()] bombs
  let attempt = do
        action <- atomicModifyIORef bombsRef \(b : bs) -> (bs, b)
        action
  lazyBracket
    attempt
    (\_ -> pure ())
    ( \Resource {accessResource} -> do
        _ <- accessResource
        _ <- accessResource
        pure ()
    )
  pure ()

noAcquisition :: Assertion
noAcquisition = do
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
      (throwIO (userError "should not run"))
      (\_ -> writeIORef ref Disposed)
      ( \Resource {accessResource, controlResource} -> do
          controlResource (\_ -> modifyIORef opsRef (<> [TestOpA]))
          pure ()
      )
  refMustBe "release function must not run" NotYetAcquired
  opsRefMustBe "no ops should trigger" []
  pure ()

noAcquisitionEx :: Assertion
noAcquisitionEx = do
  e <- try @DummyEx do
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
        (throwIO (userError "should not run"))
        (\_ -> writeIORef ref Disposed)
        ( \Resource {accessResource, controlResource} -> do
            controlResource (\_ -> modifyIORef opsRef (<> [TestOpA]))
            throwIO DummyEx
            pure ()
        )
    refMustBe "release function must not run" NotYetAcquired
    opsRefMustBe "no ops should trigger" []
    pure ()
  case e of 
      Left DummyEx -> pure ()
      Right _ -> assertFailure "Exception should have bubbled up"

main :: IO ()
main = defaultMain tests
