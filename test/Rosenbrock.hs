{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import Numeric.MCMC.Metropolis

rosenbrock :: [Double] -> Double
rosenbrock [x0, x1] = negate (5  *(x1 - x0 ^ 2) ^ 2 + 0.05 * (1 - x0) ^ 2)

main :: IO ()
main = withSystemRandom . asGenIO $ \gen -> do
  _ <- chain 50 1 [0, 0] rosenbrock gen
  mcmc 50 1 [0, 0] rosenbrock gen

