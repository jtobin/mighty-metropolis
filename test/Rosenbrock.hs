
module Rosenbrock where

import Numeric.MCMC.Metropolis
import qualified System.Random.MWC.Probability as MWC
import Data.Vector.Unboxed (Vector, unsafeIndex)

rosenbrock :: Vector Double -> Double
rosenbrock xs = (-1)*(5*(x1 - x0^2)^2 + 0.05*(1 - x0)^2) where
  x0 = unsafeIndex xs 0
  x1 = unsafeIndex xs 1

main :: IO ()
main = MWC.withSystemRandom . MWC.asGenIO $ mcmc 100000 1 [0, 0] rosenbrock

