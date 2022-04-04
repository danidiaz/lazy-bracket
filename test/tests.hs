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
{-# LANGUAGE DataKinds #-}

module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.IORef
import LazyBracket

tests :: TestTree
tests =
  testGroup
    "All"
    [
        testCase "foo" foo
    ]

data TestResourceState =
        NotYetAcquired
        | AlreadyAcquired
        | ControlOpPerformed
        | Disposed

foo :: Assertion
foo = do
    ref <- newIORef (1::Int)
    _ <- lazyBracket
        (modifyIORef ref succ)
        (\_ -> modifyIORef ref succ)
        (\Resource {accessResource, controlResource} -> pure ())
    pure ()

main :: IO ()
main = defaultMain tests
