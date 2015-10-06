
module Main where

import Numeric.MCMC.Metropolis

bnn :: [Double] -> Double
bnn [x0, x1] = -0.5 * (x0 ^ 2 * x1 ^ 2 + x0 ^ 2 + x1 ^ 2 - 8 * x0 - 8 * x1)

main :: IO ()
main = withSystemRandom . asGenIO $ mcmc 10000 1 [0, 0] bnn

