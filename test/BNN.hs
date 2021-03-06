{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import Numeric.MCMC.Metropolis
import Data.Sequence (Seq, fromList, index)

bnn :: Seq Double -> Double
bnn xs = -0.5 * (x0 ^ 2 * x1 ^ 2 + x0 ^ 2 + x1 ^ 2 - 8 * x0 - 8 * x1) where
  x0 = index xs 0
  x1 = index xs 1

main :: IO ()
main = withSystemRandom . asGenIO $ \gen -> do
  _ <- chain 50 1 (fromList [0, 0]) bnn gen
  mcmc 50 1 (fromList [0, 0]) bnn gen

