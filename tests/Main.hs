-- |
module Main where

import Test.Tasty.PAPI
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMainPAPI $ testGroup "FIB"
  [ bench (show n) $ whnf fib n
  | n <- [6..20]
  ]


fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
