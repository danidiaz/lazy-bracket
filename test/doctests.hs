module Main (main) where

import Test.DocTest
main = doctest [
      "-ilib"
    , "lib/LazyBracket.hs"
    ]
