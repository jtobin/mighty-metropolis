{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module: Numeric.MCMC.Metropolis
-- Copyright: (c) 2015 Jared Tobin
-- License: MIT
--
-- Maintainer: Jared Tobin <jared@jtobin.ca>
-- Stability: unstable
-- Portability: ghc
--
-- You have to wake up pretty early to beat the Metropolis algorithm.
--
-- This implementation uses spherical Gaussian proposals to implement a
-- reliable and computationally inexpensive sampling routine.  It can be used
-- as a baseline from which to benchmarks other algorithms on a given problem.
--
-- The 'mcmc' function streams a trace to stdout to be processed elsewhere,
-- while the `metropolis` transition can be used for more flexible purposes,
-- such as working with samples in memory.

module Numeric.MCMC.Metropolis (
    mcmc
  , metropolis

  -- * re-exported
  , module Data.Sampling.Types
  , MWC.create
  , MWC.createSystemRandom
  , MWC.withSystemRandom
  , MWC.asGenIO
  ) where

import Control.Monad (when)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (execStateT, get, put)
import Data.Sampling.Types (Target(..), Chain(..), Transition)
import GHC.Prim (RealWorld)
import Pipes (Producer, yield, (>->), runEffect)
import qualified Pipes.Prelude as Pipes (mapM_, take)
import System.Random.MWC.Probability (Gen, Prob)
import qualified System.Random.MWC.Probability as MWC

-- Propose a state transition according to a Gaussian proposal distribution
-- with the specified standard deviation.
propose
  :: (PrimMonad m, Traversable f)
  => Double
  -> f Double
  -> Prob m (f Double)
propose radial = traverse perturb where
  perturb m = MWC.normal m radial

-- | A generic Metropolis transition operator.
metropolis
  :: (Traversable f, PrimMonad m)
  => Double
  -> Transition m (Chain (f Double) b)
metropolis radial = do
  Chain {..} <- get
  proposal <- lift (propose radial chainPosition)
  let proposalScore = lTarget chainTarget proposal
      acceptProb    = whenNaN 0 (exp (min 0 (proposalScore - chainScore)))

  accept <- lift (MWC.bernoulli acceptProb)
  when accept (put (Chain chainTarget proposalScore proposal chainTunables))

-- A Markov chain driven by the Metropolis transition operator.
chain
  :: (Traversable f, PrimMonad m)
  => Double
  -> Chain (f Double) b
  -> Gen (PrimState m)
  -> Producer (Chain (f Double) b) m ()
chain radial = loop where
  loop state prng = do
    next <- lift (MWC.sample (execStateT (metropolis radial) state) prng)
    yield next
    loop next prng

-- | Trace 'n' iterations of a Markov chain and stream them to stdout.
--
-- >>> let rosenbrock [x0, x1] = negate (5  *(x1 - x0 ^ 2) ^ 2 + 0.05 * (1 - x0) ^ 2)
-- >>> withSystemRandom . asGenIO $ mcmc 3 1 [0, 0] rosenbrock
-- 0.5000462419822702,0.5693944056267897
-- 0.5000462419822702,0.5693944056267897
-- -0.7525995304580824,1.2240725505283248
mcmc
  :: (Traversable f, Show (f Double))
  => Int
  -> Double
  -> f Double
  -> (f Double -> Double)
  -> Gen RealWorld
  -> IO ()
mcmc n radial chainPosition target gen = runEffect $
        chain radial Chain {..} gen
    >-> Pipes.take n
    >-> Pipes.mapM_ print
  where
    chainScore    = lTarget chainTarget chainPosition
    chainTunables = Nothing
    chainTarget   = Target target Nothing

-- Use a provided default value when the argument is NaN.
whenNaN :: RealFloat a => a -> a -> a
whenNaN val x
  | isNaN x   = val
  | otherwise = x
