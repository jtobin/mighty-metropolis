{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE CPP #-}
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
-- This implementation uses spherical Gaussian proposals to implement a
-- reliable and computationally inexpensive sampling routine.  It can be used
-- as a baseline from which to benchmark other algorithms for a given problem.
--
-- The 'mcmc' function streams a trace to stdout to be processed elsewhere,
-- while the `metropolis` transition can be used for more flexible purposes,
-- such as working with samples in memory.

module Numeric.MCMC.Metropolis (
    mcmc
  , chain
  , metropolis

  -- * Re-exported
  , module Data.Sampling.Types
  , MWC.create
  , MWC.createSystemRandom
  , MWC.withSystemRandom
  , MWC.asGenIO
  ) where

import Control.Monad (when, replicateM)
import Control.Monad.Codensity (lowerCodensity)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (execStateT, get, put)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Sampling.Types (Target(..), Chain(..), Transition)
#if __GLASGOW_HASKELL__ < 710
import Data.Traversable (Traversable, traverse)
#endif
import Pipes (Producer, Consumer, yield, (>->), runEffect, await)
import qualified Pipes.Prelude as Pipes (mapM_, take, map)
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

-- Drive a Markov chain via the Metropolis transition operator.
drive
  :: (Traversable f, PrimMonad m)
  => Double
  -> Chain (f Double) b
  -> Gen (PrimState m)
  -> Producer (Chain (f Double) b) m c
drive radial = loop where
  loop state prng = do
    next <- lift (MWC.sample (execStateT (metropolis radial) state) prng)
    yield next
    loop next prng

-- | Trace 'n' iterations of a Markov chain and collect the results in a list.
--
-- >>> let rosenbrock [x0, x1] = negate (5  *(x1 - x0 ^ 2) ^ 2 + 0.05 * (1 - x0) ^ 2)
-- >>> results <- withSystemRandom . asGenIO $ chain 3 1 [0, 0] rosenbrock
-- >>> mapM_ print results
-- [0.0,0.0]
-- [1.4754117657794871e-2,0.5033208261760778]
-- [3.8379699517007895e-3,0.24627131099479127]
chain
  :: (PrimMonad m, Traversable f)
  => Int
  -> Double
  -> f Double
  -> (f Double -> Double)
  -> Gen (PrimState m)
  -> m [f Double]
chain n radial position target gen = runEffect $
        drive radial origin gen
    >-> Pipes.map chainPosition
    >-> collect n
  where
    ctarget = Target target Nothing

    origin = Chain {
        chainScore    = lTarget ctarget position
      , chainTunables = Nothing
      , chainTarget   = ctarget
      , chainPosition = position
      }

    collect :: Monad m => Int -> Consumer a m [a]
    collect size = lowerCodensity $
      replicateM size (lift Pipes.await)

-- | Trace 'n' iterations of a Markov chain and stream them to stdout.
--
-- >>> let rosenbrock [x0, x1] = negate (5  *(x1 - x0 ^ 2) ^ 2 + 0.05 * (1 - x0) ^ 2)
-- >>> withSystemRandom . asGenIO $ mcmc 3 1 [0, 0] rosenbrock
-- 0.5000462419822702,0.5693944056267897
-- 0.5000462419822702,0.5693944056267897
-- -0.7525995304580824,1.2240725505283248
mcmc
  :: (MonadIO m, PrimMonad m, Traversable f, Show (f Double))
  => Int
  -> Double
  -> f Double
  -> (f Double -> Double)
  -> Gen (PrimState m)
  -> m ()
mcmc n radial chainPosition target gen = runEffect $
        drive radial Chain {..} gen
    >-> Pipes.take n
    >-> Pipes.mapM_ (liftIO . print)
  where
    chainScore    = lTarget chainTarget chainPosition
    chainTunables = Nothing
    chainTarget   = Target target Nothing

-- Use a provided default value when the argument is NaN.
whenNaN :: RealFloat a => a -> a -> a
whenNaN val x
  | isNaN x   = val
  | otherwise = x
