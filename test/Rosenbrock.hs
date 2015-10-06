
module Main where

import Numeric.MCMC.Metropolis
import qualified System.Random.MWC.Probability as MWC

rosenbrock :: [Double] -> Double
rosenbrock xs = (-1)*(5*(x1 - x0^2)^2 + 0.05*(1 - x0)^2) where
  x0 = head xs
  x1 = xs !! 1

main :: IO ()
main = MWC.withSystemRandom . MWC.asGenIO $
  mcmc 10000 1 [0, 0] rosenbrock

