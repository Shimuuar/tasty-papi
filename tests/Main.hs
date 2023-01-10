-- |
module Main where

import Test.Tasty.PAPI

main :: IO ()
main = defaultMain
  [ bench (show n) $ whnf fib n
  | n <- [6..20]
  ]

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
