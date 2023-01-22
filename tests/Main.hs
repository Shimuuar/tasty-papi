-- |
module Main where

import Control.Monad
import Data.IORef
import Data.Function
import Test.Tasty.PAPI

main :: IO ()
main = defaultMain
  [ bgroup "Fib"
    [ bench (show n) $ whnf fib n
    | n <- [6..10]
    ]
  , bgroup "Parameter evaluation"
    [ bench "Eval1" $ whnf sum constant
    , bench "Eval2" $ whnf sum constant
    ]
  , bgroup "Misc"
    [ -- Show performance regression between 9.2 and 9.4
      bench "IORef regression" $ whnfIO $ do
        ref <- newIORef (1000 :: Int)
        fix $ \loop -> do
          x <- readIORef ref
          unless (x == 0) $ do
            writeIORef ref $ x - 1
            loop
        readIORef ref
    ]
  ]

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

constant :: [Int]
constant = [1 .. 1000]
